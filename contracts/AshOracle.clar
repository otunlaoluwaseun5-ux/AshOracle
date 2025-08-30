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
