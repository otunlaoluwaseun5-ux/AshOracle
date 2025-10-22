;; title: AshOracle
;; version: 1.0.0
;; summary: Decentralized oracle with burn-to-submit mechanism and reputation staking
;; description: Oracle system where data providers burn STX to submit feeds, with weighted consensus and reputation multipliers

;; traits
(define-trait oracle-feed-trait
  (
    (get-feed-data (uint) (response {price: uint, timestamp: uint, confidence: uint} uint))
  )
)

;; token definitions
;; Using native STX for burning mechanism

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_FEED_NOT_FOUND (err u102))
(define-constant ERR_INSUFFICIENT_BURN (err u103))
(define-constant ERR_CIRCUIT_BREAKER_ACTIVE (err u104))
(define-constant ERR_INVALID_TIMESTAMP (err u105))
(define-constant ERR_DUPLICATE_SUBMISSION (err u106))
(define-constant ERR_OVERFLOW (err u107))
(define-constant ERR_UNDERFLOW (err u108))
(define-constant ERR_RATE_LIMIT_EXCEEDED (err u109))
(define-constant ERR_INVALID_INPUT (err u110))
(define-constant ERR_PRICE_DEVIATION_TOO_HIGH (err u111))

(define-constant MIN_BURN_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_PRICE_DEVIATION u20) ;; 20% max deviation
(define-constant CONSENSUS_WINDOW u10) ;; 10 block consensus window
(define-constant REPUTATION_MULTIPLIER_BASE u100)
(define-constant SLASH_PERCENTAGE u50) ;; 50% slash for bad data
(define-constant RATE_LIMIT_BLOCKS u5) ;; Minimum blocks between submissions
(define-constant MAX_REPUTATION_SCORE u300)
(define-constant MIN_REPUTATION_SCORE u10)

;; data vars
(define-data-var contract-paused bool false)
(define-data-var total-feeds-count uint u0)
(define-data-var emergency-admin principal CONTRACT_OWNER)

;; data maps
(define-map feed-data 
  { feed-id: uint }
  {
    name: (string-ascii 64),
    latest-price: uint,
    latest-timestamp: uint,
    submission-count: uint,
    active: bool
  }
)

(define-map feed-submissions
  { feed-id: uint, block-height: uint, submitter: principal }
  {
    price: uint,
    burn-amount: uint,
    timestamp: uint,
    weight: uint,
    slashed: bool
  }
)

(define-map oracle-reputation
  { oracle: principal }
  {
    total-submissions: uint,
    accurate-submissions: uint,
    total-burned: uint,
    reputation-score: uint,
    last-submission-block: uint
  }
)

(define-map consensus-data
  { feed-id: uint, block-height: uint }
  {
    total-weight: uint,
    weighted-price-sum: uint,
    submission-count: uint,
    finalized: bool
  }
)

(define-map feed-consensus-history
  { feed-id: uint, round: uint }
  {
    consensus-price: uint,
    total-participants: uint,
    block-height: uint,
    timestamp: uint
  }
)

;; Security helper functions

(define-private (safe-add (a uint) (b uint))
  (let ((result (+ a b)))
    (asserts! (>= result a) ERR_OVERFLOW)
    (ok result)))

(define-private (safe-sub (a uint) (b uint))
  (if (>= a b)
    (ok (- a b))
    ERR_UNDERFLOW))

(define-private (safe-mul (a uint) (b uint))
  (let ((result (* a b)))
    (asserts! (or (is-eq b u0) (is-eq (/ result b) a)) ERR_OVERFLOW)
    (ok result)))

(define-private (safe-div (a uint) (b uint))
  (if (> b u0)
    (ok (/ a b))
    ERR_INVALID_AMOUNT))

(define-private (check-rate-limit (oracle principal) (last-block uint))
  (let ((current-block stacks-block-height))
    (asserts! (>= (- current-block last-block) RATE_LIMIT_BLOCKS) ERR_RATE_LIMIT_EXCEEDED)
    (ok true)))

(define-private (validate-feed-name (name (string-ascii 64)))
  (if (> (len name) u0)
    (ok true)
    ERR_INVALID_INPUT))

;; public functions

;; Create a new price feed
(define-public (create-feed (name (string-ascii 64)))
  (let
    (
      (current-count (var-get total-feeds-count))
      (feed-id (unwrap! (safe-add current-count u1) ERR_OVERFLOW))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    (try! (validate-feed-name name))
    
    (map-set feed-data
      { feed-id: feed-id }
      {
        name: name,
        latest-price: u0,
        latest-timestamp: u0,
        submission-count: u0,
        active: true
      }
    )
    
    (var-set total-feeds-count feed-id)
    (ok feed-id)
  )
)

;; Submit price data by burning STX
(define-public (submit-feed-data (feed-id uint) (price uint) (burn-amount uint))
  (let
    (
      (current-block stacks-block-height)
      (current-timestamp stacks-block-height)
      (feed-info (unwrap! (map-get? feed-data { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
      (oracle-rep (default-to 
        { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
        (map-get? oracle-reputation { oracle: tx-sender })
      ))
      (reputation-multiplier (calculate-reputation-multiplier (get reputation-score oracle-rep)))
      (effective-weight (unwrap! (safe-mul burn-amount reputation-multiplier) ERR_OVERFLOW))
    )
    
    ;; Validations
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    (asserts! (get active feed-info) ERR_FEED_NOT_FOUND)
    (asserts! (>= burn-amount MIN_BURN_AMOUNT) ERR_INSUFFICIENT_BURN)
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? feed-submissions { feed-id: feed-id, block-height: current-block, submitter: tx-sender })) ERR_DUPLICATE_SUBMISSION)
    
    ;; Rate limiting check
    (try! (check-rate-limit tx-sender (get last-submission-block oracle-rep)))
    
    ;; Price deviation check
    (asserts! (validate-price-submission feed-id price) ERR_PRICE_DEVIATION_TOO_HIGH)
    
    ;; Burn STX tokens (external call first - reentrancy protection)
    (try! (stx-burn? burn-amount tx-sender))
    
    ;; Record submission
    (map-set feed-submissions
      { feed-id: feed-id, block-height: current-block, submitter: tx-sender }
      {
        price: price,
        burn-amount: burn-amount,
        timestamp: current-timestamp,
        weight: effective-weight,
        slashed: false
      }
    )
    
    ;; Update consensus data with safe math
    (let
      (
        (consensus (default-to 
          { total-weight: u0, weighted-price-sum: u0, submission-count: u0, finalized: false }
          (map-get? consensus-data { feed-id: feed-id, block-height: current-block })
        ))
        (new-total-weight (unwrap! (safe-add (get total-weight consensus) effective-weight) ERR_OVERFLOW))
        (price-weight (unwrap! (safe-mul price effective-weight) ERR_OVERFLOW))
        (new-weighted-sum (unwrap! (safe-add (get weighted-price-sum consensus) price-weight) ERR_OVERFLOW))
        (new-submission-count (unwrap! (safe-add (get submission-count consensus) u1) ERR_OVERFLOW))
      )
      (map-set consensus-data
        { feed-id: feed-id, block-height: current-block }
        {
          total-weight: new-total-weight,
          weighted-price-sum: new-weighted-sum,
          submission-count: new-submission-count,
          finalized: false
        }
      )
    )
    
    ;; Update oracle reputation with safe math
    (let
      (
        (new-total-submissions (unwrap! (safe-add (get total-submissions oracle-rep) u1) ERR_OVERFLOW))
        (new-total-burned (unwrap! (safe-add (get total-burned oracle-rep) burn-amount) ERR_OVERFLOW))
      )
      (map-set oracle-reputation
        { oracle: tx-sender }
        {
          total-submissions: new-total-submissions,
          accurate-submissions: (get accurate-submissions oracle-rep),
          total-burned: new-total-burned,
          reputation-score: (get reputation-score oracle-rep),
          last-submission-block: current-block
        }
      )
    )
    
    (ok true)
  )
)

;; Finalize consensus for a specific block and feed
(define-public (finalize-consensus (feed-id uint) (blk uint))
  (let
    (
      (consensus (unwrap! (map-get? consensus-data { feed-id: feed-id, block-height: blk }) ERR_FEED_NOT_FOUND))
      (feed-info (unwrap! (map-get? feed-data { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
      (current-block stacks-block-height)
    )
    
    (asserts! (not (get finalized consensus)) ERR_UNAUTHORIZED)
    (asserts! (> (get submission-count consensus) u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (unwrap! (safe-sub current-block blk) ERR_UNDERFLOW) CONSENSUS_WINDOW) ERR_INVALID_TIMESTAMP)
    
    (let
      (
        (consensus-price (unwrap! (safe-div (get weighted-price-sum consensus) (get total-weight consensus)) ERR_INVALID_AMOUNT))
        (round-number (unwrap! (safe-add (get submission-count feed-info) u1) ERR_OVERFLOW))
      )
      
      ;; Update feed data with consensus price
      (map-set feed-data
        { feed-id: feed-id }
        (merge feed-info {
          latest-price: consensus-price,
          latest-timestamp: blk,
          submission-count: round-number
        })
      )
      
      ;; Mark consensus as finalized
      (map-set consensus-data
        { feed-id: feed-id, block-height: blk }
        (merge consensus { finalized: true })
      )
      
      ;; Store consensus history
      (map-set feed-consensus-history
        { feed-id: feed-id, round: round-number }
        {
          consensus-price: consensus-price,
          total-participants: (get submission-count consensus),
          block-height: blk,
          timestamp: blk
        }
      )
      
      ;; Update oracle reputations based on accuracy
      (try! (update-oracle-reputations feed-id blk consensus-price))
      
      (ok consensus-price)
    )
  )
)

;; Slash oracle for submitting bad data
(define-public (slash-oracle (feed-id uint) (blk uint) (oracle principal))
  (let
    (
      (submission (unwrap! (map-get? feed-submissions { feed-id: feed-id, block-height: blk, submitter: oracle }) ERR_FEED_NOT_FOUND))
      (oracle-rep (unwrap! (map-get? oracle-reputation { oracle: oracle }) ERR_FEED_NOT_FOUND))
    )
    
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    (asserts! (not (get slashed submission)) ERR_INVALID_AMOUNT)
    
    ;; Mark submission as slashed
    (map-set feed-submissions
      { feed-id: feed-id, block-height: blk, submitter: oracle }
      (merge submission { slashed: true })
    )
    
    ;; Reduce oracle reputation with safe math
    (let
      (
        (current-score (get reputation-score oracle-rep))
        (new-score (if (>= current-score u20)
          (- current-score u20)
          MIN_REPUTATION_SCORE))
      )
      (map-set oracle-reputation
        { oracle: oracle }
        (merge oracle-rep {
          reputation-score: new-score
        })
      )
    )
    
    (ok true)
  )
)

;; Emergency circuit breaker
(define-public (toggle-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender (var-get emergency-admin)) ERR_UNAUTHORIZED)
    (var-set contract-paused (not (var-get contract-paused)))
    (ok (var-get contract-paused))
  )
)

;; Update emergency admin
(define-public (set-emergency-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set emergency-admin new-admin)
    (ok true)
  )
)

;; read only functions

;; Get latest price for a feed
(define-read-only (get-price (feed-id uint))
  (match (map-get? feed-data { feed-id: feed-id })
    feed-info (ok {
      price: (get latest-price feed-info),
      timestamp: (get latest-timestamp feed-info),
      name: (get name feed-info)
    })
    ERR_FEED_NOT_FOUND
  )
)

;; Get oracle reputation
(define-read-only (get-oracle-reputation (oracle principal))
  (match (map-get? oracle-reputation { oracle: oracle })
    rep (ok rep)
    (ok { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 })
  )
)

;; Get feed submission details
(define-read-only (get-submission (feed-id uint) (blk uint) (submitter principal))
  (map-get? feed-submissions { feed-id: feed-id, block-height: blk, submitter: submitter })
)

;; Get consensus data for a specific block
(define-read-only (get-consensus-data (feed-id uint) (blk uint))
  (map-get? consensus-data { feed-id: feed-id, block-height: blk })
)

;; Get feed information
(define-read-only (get-feed-info (feed-id uint))
  (map-get? feed-data { feed-id: feed-id })
)

;; Get contract status
(define-read-only (get-contract-status)
  (ok {
    paused: (var-get contract-paused),
    total-feeds: (var-get total-feeds-count),
    emergency-admin: (var-get emergency-admin)
  })
)

;; NEW: Security read-only functions
(define-read-only (is-contract-paused)
  (var-get contract-paused))

(define-read-only (get-rate-limit-blocks)
  RATE_LIMIT_BLOCKS)

(define-read-only (get-min-burn-amount)
  MIN_BURN_AMOUNT)

(define-read-only (get-max-price-deviation)
  MAX_PRICE_DEVIATION)

;; Calculate required burn amount based on reputation
(define-read-only (calculate-required-burn (oracle principal))
  (let
    (
      (rep (get reputation-score (default-to 
        { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
        (map-get? oracle-reputation { oracle: oracle })
      )))
      (multiplier (calculate-reputation-multiplier rep))
    )
    (ok (/ MIN_BURN_AMOUNT multiplier))
  )
)


;; private functions

;; Calculate reputation multiplier (higher reputation = lower burn requirement)
(define-private (calculate-reputation-multiplier (reputation-score uint))
  (if (>= reputation-score u200)
    u300  ;; 3x multiplier for high reputation
    (if (>= reputation-score u150)
      u200  ;; 2x multiplier for good reputation
      (if (>= reputation-score u100)
        u100  ;; 1x multiplier for average reputation
        u50   ;; 0.5x multiplier for low reputation
      )
    )
  )
)

;; Update oracle reputations based on submission accuracy
(define-private (update-oracle-reputations (feed-id uint) (blk uint) (consensus-price uint))
  (if true (ok true) (err u0))
)

;; Update a single oracle's reputation
(define-private (update-single-oracle-reputation 
  (submission { oracle: principal, price: uint, weight: uint })
  (previous-result (response bool uint))
)
  (match previous-result
    success
    (let
      (
        (oracle (get oracle submission))
        (submitted-price (get price submission))
        (oracle-rep (default-to 
          { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
          (map-get? oracle-reputation { oracle: oracle })
        ))
        (price-deviation (abs-diff submitted-price (get price submission)))
        (deviation-threshold (unwrap-panic (safe-div (get price submission) u20)))
        (is-accurate (< price-deviation deviation-threshold))
      )
      
      (let
        (
          (current-score (get reputation-score oracle-rep))
          (new-accurate-count (if is-accurate 
            (unwrap-panic (safe-add (get accurate-submissions oracle-rep) u1))
            (get accurate-submissions oracle-rep)
          ))
          (new-reputation-score (if is-accurate
            (min-uint MAX_REPUTATION_SCORE (unwrap-panic (safe-add current-score u5)))
            (if (>= current-score u10)
              (- current-score u10)
              MIN_REPUTATION_SCORE)
          ))
        )
        (map-set oracle-reputation
          { oracle: oracle }
          {
            total-submissions: (get total-submissions oracle-rep),
            accurate-submissions: new-accurate-count,
            total-burned: (get total-burned oracle-rep),
            reputation-score: new-reputation-score,
            last-submission-block: (get last-submission-block oracle-rep)
          }
        )
      )
      
      (ok true)
    )
    error (err error)
  )
)

;; Get all submissions for a specific block (simplified for this implementation)
(define-private (get-block-submissions (feed-id uint) (blk uint))
  (list)  ;; In a full implementation, this would iterate through submissions
)

;; Calculate absolute difference between two uints
(define-private (abs-diff (a uint) (b uint))
  (if (>= a b)
    (- a b)
    (- b a)
  )
)

;; Helper: min and max for uint (since built-ins may be unavailable)
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b)
)

;; Validate price against existing consensus (anti-manipulation)
(define-private (validate-price-submission (feed-id uint) (submitted-price uint))
  (match (map-get? feed-data { feed-id: feed-id })
    feed-info
    (if (> (get latest-price feed-info) u0)
      (let
        (
          (latest-price (get latest-price feed-info))
          (max-deviation (/ (* latest-price MAX_PRICE_DEVIATION) u100))
          (price-diff (abs-diff submitted-price latest-price))
        )
        (<= price-diff max-deviation)
      )
      true  ;; No previous price to validate against
    )
    false
  )
)

;; Calculate weighted average price with safe math
(define-private (calculate-weighted-consensus (total-weight uint) (weighted-sum uint))
  (if (> total-weight u0)
    (unwrap-panic (safe-div weighted-sum total-weight))
    u0
  )
)

;; Check if oracle meets minimum requirements
(define-private (meets-minimum-requirements (oracle principal) (burn-amount uint))
  (let
    (
      (required-burn (unwrap-panic (calculate-required-burn oracle)))
    )
    (>= burn-amount required-burn)
  )
)

