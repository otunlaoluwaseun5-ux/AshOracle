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

(define-constant MIN_BURN_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_PRICE_DEVIATION u20) ;; 20% max deviation
(define-constant CONSENSUS_WINDOW u10) ;; 10 block consensus window
(define-constant REPUTATION_MULTIPLIER_BASE u100)
(define-constant SLASH_PERCENTAGE u50) ;; 50% slash for bad data

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


;; public functions

;; Create a new price feed
(define-public (create-feed (name (string-ascii 64)))
  (let
    (
      (feed-id (+ (var-get total-feeds-count) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    
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
      (current-block block-height)
      (current-timestamp (unwrap! (get-block-info? time current-block) ERR_INVALID_TIMESTAMP))
      (feed-info (unwrap! (map-get? feed-data { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
      (oracle-rep (default-to 
        { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
        (map-get? oracle-reputation { oracle: tx-sender })
      ))
      (reputation-multiplier (calculate-reputation-multiplier (get reputation-score oracle-rep)))
      (effective-weight (* burn-amount reputation-multiplier))
    )
    
    ;; Validations
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    (asserts! (get active feed-info) ERR_FEED_NOT_FOUND)
    (asserts! (>= burn-amount MIN_BURN_AMOUNT) ERR_INSUFFICIENT_BURN)
    (asserts! (> price u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? feed-submissions { feed-id: feed-id, block-height: current-block, submitter: tx-sender })) ERR_DUPLICATE_SUBMISSION)
    
    ;; Burn STX tokens
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
    
    ;; Update consensus data
    (let
      (
        (consensus (default-to 
          { total-weight: u0, weighted-price-sum: u0, submission-count: u0, finalized: false }
          (map-get? consensus-data { feed-id: feed-id, block-height: current-block })
        ))
      )
      (map-set consensus-data
        { feed-id: feed-id, block-height: current-block }
        {
          total-weight: (+ (get total-weight consensus) effective-weight),
          weighted-price-sum: (+ (get weighted-price-sum consensus) (* price effective-weight)),
          submission-count: (+ (get submission-count consensus) u1),
          finalized: false
        }
      )
    )
    
    ;; Update oracle reputation
    (map-set oracle-reputation
      { oracle: tx-sender }
      {
        total-submissions: (+ (get total-submissions oracle-rep) u1),
        accurate-submissions: (get accurate-submissions oracle-rep),
        total-burned: (+ (get total-burned oracle-rep) burn-amount),
        reputation-score: (get reputation-score oracle-rep),
        last-submission-block: current-block
      }
    )
    
    (ok true)
  )
)

;; Finalize consensus for a specific block and feed
(define-public (finalize-consensus (feed-id uint) (block-height uint))
  (let
    (
      (consensus (unwrap! (map-get? consensus-data { feed-id: feed-id, block-height: block-height }) ERR_FEED_NOT_FOUND))
      (feed-info (unwrap! (map-get? feed-data { feed-id: feed-id }) ERR_FEED_NOT_FOUND))
    )
    
    (asserts! (not (get finalized consensus)) ERR_UNAUTHORIZED)
    (asserts! (> (get submission-count consensus) u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (- block-height block-height) CONSENSUS_WINDOW) ERR_INVALID_TIMESTAMP)
    
    (let
      (
        (consensus-price (/ (get weighted-price-sum consensus) (get total-weight consensus)))
        (round-number (+ (get submission-count feed-info) u1))
      )
      
      ;; Update feed data with consensus price
      (map-set feed-data
        { feed-id: feed-id }
        (merge feed-info {
          latest-price: consensus-price,
          latest-timestamp: (unwrap! (get-block-info? time block-height) ERR_INVALID_TIMESTAMP),
          submission-count: round-number
        })
      )
      
      ;; Mark consensus as finalized
      (map-set consensus-data
        { feed-id: feed-id, block-height: block-height }
        (merge consensus { finalized: true })
      )
      
      ;; Store consensus history
      (map-set feed-consensus-history
        { feed-id: feed-id, round: round-number }
        {
          consensus-price: consensus-price,
          total-participants: (get submission-count consensus),
          block-height: block-height,
          timestamp: (unwrap! (get-block-info? time block-height) ERR_INVALID_TIMESTAMP)
        }
      )
      
      ;; Update oracle reputations based on accuracy
      (try! (update-oracle-reputations feed-id block-height consensus-price))
      
      (ok consensus-price)
    )
  )
)

;; Slash oracle for submitting bad data
(define-public (slash-oracle (feed-id uint) (block-height uint) (oracle principal))
  (let
    (
      (submission (unwrap! (map-get? feed-submissions { feed-id: feed-id, block-height: block-height, submitter: oracle }) ERR_FEED_NOT_FOUND))
      (oracle-rep (unwrap! (map-get? oracle-reputation { oracle: oracle }) ERR_FEED_NOT_FOUND))
    )
    
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (not (get slashed submission)) ERR_INVALID_AMOUNT)
    
    ;; Mark submission as slashed
    (map-set feed-submissions
      { feed-id: feed-id, block-height: block-height, submitter: oracle }
      (merge submission { slashed: true })
    )
    
    ;; Reduce oracle reputation
    (map-set oracle-reputation
      { oracle: oracle }
      (merge oracle-rep {
        reputation-score: (max u10 (- (get reputation-score oracle-rep) u20))
      })
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
(define-read-only (get-submission (feed-id uint) (block-height uint) (submitter principal))
  (map-get? feed-submissions { feed-id: feed-id, block-height: block-height, submitter: submitter })
)

;; Get consensus data for a specific block
(define-read-only (get-consensus-data (feed-id uint) (block-height uint))
  (map-get? consensus-data { feed-id: feed-id, block-height: block-height })
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

;; Calculate required burn amount based on reputation
(define-read-only (calculate-required-burn (oracle principal))
  (let
    (
      (rep (default-to u100 (get reputation-score (default-to 
        { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
        (map-get? oracle-reputation { oracle: oracle })
      ))))
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
(define-private (update-oracle-reputations (feed-id uint) (block-height uint) (consensus-price uint))
  (let
    (
      (submissions-list (get-block-submissions feed-id block-height))
    )
    (fold update-single-oracle-reputation submissions-list (ok true))
  )
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
        (is-accurate (< price-deviation (/ (get price submission) u20))) ;; Within 5% deviation
      )
      
      (map-set oracle-reputation
        { oracle: oracle }
        {
          total-submissions: (get total-submissions oracle-rep),
          accurate-submissions: (if is-accurate 
            (+ (get accurate-submissions oracle-rep) u1)
            (get accurate-submissions oracle-rep)
          ),
          total-burned: (get total-burned oracle-rep),
          reputation-score: (if is-accurate
            (min u300 (+ (get reputation-score oracle-rep) u5))
            (max u10 (- (get reputation-score oracle-rep) u10))
          ),
          last-submission-block: (get last-submission-block oracle-rep)
        }
      )
      
      (ok true)
    )
    error (err error)
  )
)

;; Get all submissions for a specific block (simplified for this implementation)
(define-private (get-block-submissions (feed-id uint) (block-height uint))
  (list)  ;; In a full implementation, this would iterate through submissions
)

;; Calculate absolute difference between two uints
(define-private (abs-diff (a uint) (b uint))
  (if (>= a b)
    (- a b)
    (- b a)
  )
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

;; Calculate weighted average price
(define-private (calculate-weighted-consensus (total-weight uint) (weighted-sum uint))
  (if (> total-weight u0)
    (/ weighted-sum total-weight)
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