;; Teacher Feedback and Rating Smart Contract
;; A transparent system for rating teachers and lessons

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-rating (err u101))
(define-constant err-teacher-not-found (err u102))
(define-constant err-already-rated (err u103))

;; Define data structures
;; Rating structure: rating (1-5), feedback text, timestamp
(define-map teacher-ratings
  {teacher: principal, student: principal}
  {rating: uint, feedback: (string-ascii 500), block-height: uint})

;; Teacher aggregate data: total ratings, sum of ratings, average
(define-map teacher-stats
  principal
  {total-ratings: uint, rating-sum: uint, average-rating: uint})

;; Function 1: Submit rating and feedback for a teacher
(define-public (rate-teacher (teacher principal) (rating uint) (feedback (string-ascii 500)))
  (let (
    (rating-key {teacher: teacher, student: tx-sender})
    (current-stats (default-to {total-ratings: u0, rating-sum: u0, average-rating: u0}
                               (map-get? teacher-stats teacher)))
  )
    ;; Validate rating is between 1 and 5
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)

    ;; Check if student has already rated this teacher
    (asserts! (is-none (map-get? teacher-ratings rating-key)) err-already-rated)

    ;; Store the rating and feedback
    (map-set teacher-ratings rating-key
      {
        rating: rating,
        feedback: feedback,
        block-height: stacks-block-height
      })

    ;; Update teacher statistics
    (let (
      (new-total (+ (get total-ratings current-stats) u1))
      (new-sum (+ (get rating-sum current-stats) rating))
      (new-average (/ new-sum new-total))
    )
      (map-set teacher-stats teacher
        {
          total-ratings: new-total,
          rating-sum: new-sum,
          average-rating: new-average
        })
    )

    ;; Emit event for transparency
    (print {
      event: "teacher-rated",
      teacher: teacher,
      student: tx-sender,
      rating: rating,
      block-height: stacks-block-height
    })

    (ok true)))

;; Function 2: Get teacher ratings and statistics
(define-read-only (get-teacher-info (teacher principal))
  (let (
    (stats (map-get? teacher-stats teacher))
  )
    (match stats
      teacher-data (ok {
        teacher: teacher,
        total-ratings: (get total-ratings teacher-data),
        average-rating: (get average-rating teacher-data),
        rating-sum: (get rating-sum teacher-data)
      })
      (ok {
        teacher: teacher,
        total-ratings: u0,
        average-rating: u0,
        rating-sum: u0
      }))))

;; Additional helper function to get specific rating details
(define-read-only (get-rating-details (teacher principal) (student principal))
  (let (
    (rating-key {teacher: teacher, student: student})
    (rating-data (map-get? teacher-ratings rating-key))
  )
    (match rating-data
      data (ok (some {
        rating: (get rating data),
        feedback: (get feedback data),
        block-height: (get block-height data)
      }))
      (ok none))))

;; Get all ratings count for transparency
(define-read-only (get-total-ratings-count (teacher principal))
  (let (
    (stats (map-get? teacher-stats teacher))
  )
    (match stats
      teacher-data (ok (get total-ratings teacher-data))
      (ok u0))))