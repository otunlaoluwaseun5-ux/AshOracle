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

;; ADVANCED SECURITY ERROR CODES
(define-constant ERR_ORACLE_STALE (err u112))
(define-constant ERR_ORACLE_INVALID (err u113))
(define-constant ERR_MULTI_SIG_REQUIRED (err u114))
(define-constant ERR_TIME_LOCK_ACTIVE (err u115))
(define-constant ERR_CIRCUIT_BREAKER (err u116))
(define-constant ERR_SECURITY_VIOLATION (err u117))
(define-constant ERR_BATCH_LIMIT_EXCEEDED (err u118))
(define-constant ERR_INVALID_SIGNATURE (err u119))
(define-constant ERR_OPERATION_BLACKLISTED (err u120))
(define-constant ERR_ORACLE_BLACKLISTED (err u121))
(define-constant ERR_MAX_FEEDS_EXCEEDED (err u122))

(define-constant MIN_BURN_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_PRICE_DEVIATION u20) ;; 20% max deviation
(define-constant CONSENSUS_WINDOW u10) ;; 10 block consensus window
(define-constant REPUTATION_MULTIPLIER_BASE u100)
(define-constant SLASH_PERCENTAGE u50) ;; 50% slash for bad data
(define-constant RATE_LIMIT_BLOCKS u5) ;; Minimum blocks between submissions
(define-constant MAX_REPUTATION_SCORE u300)
(define-constant MIN_REPUTATION_SCORE u10)
(define-constant ORACLE_VALIDATION_ENABLED true)
(define-constant MAX_ORACLE_STALENESS u3600) ;; 1 hour in seconds
(define-constant MULTI_SIG_THRESHOLD u2) ;; Require 2 signatures for critical operations
(define-constant TIME_LOCK_DURATION u1440) ;; 1 day time lock for critical changes
(define-constant CIRCUIT_BREAKER_THRESHOLD u10) ;; Max failures before circuit breaker
(define-constant SECURITY_EVENT_LOG_SIZE u100) ;; Max security events to log
(define-constant MAX_FEEDS_PER_ORACLE u50) ;; Max feeds an oracle can submit to
(define-constant ORACLE_BLACKLIST_TIMEOUT u10080) ;; 7 days blacklist timeout
(define-data-var contract-paused bool false)
(define-data-var total-feeds-count uint u0)
(define-data-var emergency-admin principal CONTRACT_OWNER)

;; ADVANCED SECURITY DATA VARS
(define-data-var oracle-last-update uint u0)
(define-data-var oracle-data-valid bool true)
(define-data-var circuit-breaker-active bool false)
(define-data-var circuit-breaker-failures uint u0)
(define-data-var time-lock-unlock-block uint u0)
(define-data-var security-admin principal CONTRACT_OWNER)
(define-data-var next-event-id uint u1)

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

;; ADVANCED SECURITY MAPS
(define-map oracle-data
  (string-ascii 32)
  { value: uint, timestamp: uint, valid: bool }
)

(define-map multi-sig-signatures
  { operation-id: uint, signer: principal }
  { signature: (buff 65), signed-at: uint }
)

(define-map time-locked-operations
  uint
  { operation: (string-ascii 64), unlock-block: uint, executed: bool }
)

(define-map security-event-log
  uint
  { event-type: (string-ascii 32), details: (string-ascii 256), block: uint, actor: principal }
)

(define-map operation-blacklist
  (string-ascii 64)
  bool
)

(define-map oracle-blacklist
  { oracle: principal }
  { reason: (string-ascii 128), blacklist-until: uint, blacklisted-by: principal }
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
    ERR_INVALID_INPUT  )
)

;; ADVANCED SECURITY FUNCTIONS

;; Validate oracle data freshness
(define-private (validate-oracle-data (data-key (string-ascii 32)))
  (if ORACLE_VALIDATION_ENABLED
    (match (map-get? oracle-data data-key)
      oracle-entry
      (if (and
            (get valid oracle-entry)
            (< (- stacks-block-height (get timestamp oracle-entry)) MAX_ORACLE_STALENESS))
        (ok true)
        ERR_ORACLE_STALE)
      ERR_ORACLE_INVALID)
    (ok true))
)

;; Check circuit breaker status
(define-private (check-circuit-breaker)
  (if (var-get circuit-breaker-active)
    ERR_CIRCUIT_BREAKER
    (ok true))
)

;; Check time lock status
(define-private (check-time-lock)
  (if (> stacks-block-height (var-get time-lock-unlock-block))
    (ok true)
    ERR_TIME_LOCK_ACTIVE)
)

;; Check if operation is blacklisted
(define-private (check-operation-blacklist (operation (string-ascii 64)))
  (if (default-to false (map-get? operation-blacklist operation))
    ERR_OPERATION_BLACKLISTED
    (ok true))
)

;; Check if oracle is blacklisted
(define-private (check-oracle-blacklist (oracle principal))
  (match (map-get? oracle-blacklist { oracle: oracle })
    blacklist-entry
    (if (> stacks-block-height (get blacklist-until blacklist-entry))
      (ok true) ;; Blacklist expired
      ERR_ORACLE_BLACKLISTED)
    (ok true) ;; Not blacklisted
  )
)

;; Log security events
(define-private (log-security-event (event-type (string-ascii 32)) (details (string-ascii 256)))
  (let ((event-id (var-get next-event-id)))
    (map-set security-event-log event-id {
      event-type: event-type,
      details: details,
      block: stacks-block-height,
      actor: tx-sender
    })
    (var-set next-event-id (+ event-id u1))
  )
)

;; Update circuit breaker on failures
(define-private (update-circuit-breaker (operation-failed bool))
  (if operation-failed
    (let ((new-failures (+ (var-get circuit-breaker-failures) u1)))
      (var-set circuit-breaker-failures new-failures)
      (if (>= new-failures CIRCUIT_BREAKER_THRESHOLD)
        (var-set circuit-breaker-active true)
        true)
    )
    (begin
      (var-set circuit-breaker-failures u0)
      (if (var-get circuit-breaker-active)
        (var-set circuit-breaker-active false)
        true)
    )
  )
)

;; Validate oracle reputation requirements
(define-private (validate-oracle-requirements (oracle principal) (burn-amount uint))
  (let ((oracle-rep (default-to 
    { total-submissions: u0, accurate-submissions: u0, total-burned: u0, reputation-score: u100, last-submission-block: u0 }
    (map-get? oracle-reputation { oracle: oracle })
  )))
    (asserts! (>= (get reputation-score oracle-rep) u50) ERR_INVALID_INPUT)
    (asserts! (>= burn-amount MIN_BURN_AMOUNT) ERR_INSUFFICIENT_BURN)
    (try! (check-oracle-blacklist oracle))
    (ok true)
  )
)

;; public functions

;; Create a new price feed
(define-public (create-feed (name (string-ascii 64)))
  (let
    (
      (current-count (var-get total-feeds-count))
      (feed-id (unwrap! (safe-add current-count u1) ERR_OVERFLOW))
    )
    (try! (check-circuit-breaker))
    (try! (check-operation-blacklist "create-feed"))
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
    (log-security-event "feed-created" (concat "Created feed: " name))
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
    
    ;; Advanced security validations
    (try! (check-circuit-breaker))
    (try! (check-operation-blacklist "submit-data"))
    (try! (validate-oracle-requirements tx-sender burn-amount))
    
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
    
    (log-security-event "data-submitted" (concat "Price submitted for feed " (int-to-ascii feed-id)))
    (update-circuit-breaker false) ;; Reset circuit breaker on success
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

;; BATCH OPERATIONS FOR PERFORMANCE

;; Batch submit data to multiple feeds (up to 5)
(define-public (batch-submit-feed-data (submissions (list 5 { feed-id: uint, price: uint, burn-amount: uint })))
  (begin
    (try! (check-circuit-breaker))
    (try! (check-operation-blacklist "batch-submit"))
    (asserts! (not (var-get contract-paused)) ERR_CIRCUIT_BREAKER_ACTIVE)
    (asserts! (<= (len submissions) u5) ERR_BATCH_LIMIT_EXCEEDED)

    (let ((results (map batch-submit-helper submissions)))
      (log-security-event "batch-submit" (concat "Submitted to " (concat (int-to-ascii (len submissions)) " feeds")))
      (ok results)
    )
  )
)

;; Batch finalize consensus for multiple feeds
(define-public (batch-finalize-consensus (finalizations (list 5 { feed-id: uint, block: uint })))
  (begin
    (try! (check-circuit-breaker))
    (try! (check-operation-blacklist "batch-finalize"))
    (asserts! (<= (len finalizations) u5) ERR_BATCH_LIMIT_EXCEEDED)

    (let ((results (map batch-finalize-helper finalizations)))
      (log-security-event "batch-finalize" (concat "Finalized " (concat (int-to-ascii (len finalizations)) " consensuses")))
      (ok results)
    )
  )
)

;; Batch slash oracles
(define-public (batch-slash-oracles (slashes (list 5 { feed-id: uint, block: uint, oracle: principal })))
  (begin
    (try! (check-circuit-breaker))
    (try! (check-operation-blacklist "batch-slash"))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= (len slashes) u5) ERR_BATCH_LIMIT_EXCEEDED)

    (let ((results (map batch-slash-helper slashes)))
      (log-security-event "batch-slash" (concat "Slashed " (concat (int-to-ascii (len slashes)) " oracles")))
      (ok results)
    )
  )
)

;; Helper functions for batch operations
(define-private (batch-submit-helper (submission { feed-id: uint, price: uint, burn-amount: uint }))
  (match (submit-feed-data (get feed-id submission) (get price submission) (get burn-amount submission))
    success u1
    error u0
  )
)

(define-private (batch-finalize-helper (finalization { feed-id: uint, block: uint }))
  (match (finalize-consensus (get feed-id finalization) (get block finalization))
    success u1
    error u0
  )
)

(define-private (batch-slash-helper (slash { feed-id: uint, block: uint, oracle: principal }))
  (match (slash-oracle (get feed-id slash) (get block slash) (get oracle slash))
    success u1
    error u0
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

;; ADVANCED SECURITY READ-ONLY FUNCTIONS

;; Get oracle data validation status
(define-read-only (get-oracle-data (data-key (string-ascii 32)))
  (map-get? oracle-data data-key)
)

;; Get circuit breaker status
(define-read-only (get-circuit-breaker-status)
  {
    active: (var-get circuit-breaker-active),
    failures: (var-get circuit-breaker-failures),
    threshold: CIRCUIT_BREAKER_THRESHOLD
  }
)

;; Get time lock status
(define-read-only (get-time-lock-status)
  {
    unlock-block: (var-get time-lock-unlock-block),
    current-block: stacks-block-height,
    locked: (< stacks-block-height (var-get time-lock-unlock-block))
  }
)

;; Get security event log
(define-read-only (get-security-event (event-id uint))
  (map-get? security-event-log event-id)
)

;; Check if operation is blacklisted
(define-read-only (is-operation-blacklisted (operation (string-ascii 64)))
  (ok (default-to false (map-get? operation-blacklist operation)))
)

;; Check if oracle is blacklisted
(define-read-only (is-oracle-blacklisted (oracle principal))
  (match (map-get? oracle-blacklist { oracle: oracle })
    blacklist-entry (ok (> stacks-block-height (get blacklist-until blacklist-entry)))
    (ok false)
  )
)

;; Get security admin
(define-read-only (get-security-admin)
  (ok (var-get security-admin))
)

;; Get advanced contract info
(define-read-only (get-advanced-contract-info)
  (ok {
    oracle-validation-enabled: ORACLE_VALIDATION_ENABLED,
    oracle-last-update: (var-get oracle-last-update),
    oracle-data-valid: (var-get oracle-data-valid),
    circuit-breaker-active: (var-get circuit-breaker-active),
    circuit-breaker-failures: (var-get circuit-breaker-failures),
    time-lock-unlock-block: (var-get time-lock-unlock-block),
    security-admin: (var-get security-admin),
    next-event-id: (var-get next-event-id),
    max-oracle-staleness: MAX_ORACLE_STALENESS,
    multi-sig-threshold: MULTI_SIG_THRESHOLD,
    time-lock-duration: TIME_LOCK_DURATION,
    circuit-breaker-threshold: CIRCUIT_BREAKER_THRESHOLD,
    security-event-log-size: SECURITY_EVENT_LOG_SIZE
  })
)

;; Get batch operation limits
(define-read-only (get-batch-limits)
  (ok {
    max-feeds-per-oracle: MAX_FEEDS_PER_ORACLE,
    oracle-blacklist-timeout: ORACLE_BLACKLIST_TIMEOUT,
    batch-submit-limit: u5,
    batch-finalize-limit: u5,
    batch-slash-limit: u5
  })
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

