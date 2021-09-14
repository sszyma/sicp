;; Stack Exchange suggests that equivalent operations on intervals
;; give different results because the set of all intervals does not
;; form a field (an interval I with nonzero width has neither an
;; additive nor multiplicative inverse (if I does not span zero).)
;; Intuitively it makes sense, since then any operation on two
;; intervals of nonzero width must result in another interval, which
;; is the source of the problem.

;; The exercise asks if the Dependency Problem (see Wikipedia) is
;; solvable in a general case, which might not be even possible.
;; To illustrate this, consider f(x) = x^2 + x, then f([-1,1]) = [-0.25,1],
;; whereas using the interval arithmetic [-1,1]*[-1,1] + [-1,1] =
;; = [0,1] + [-1,1] = [-1,2]. "In general, it can be shown that the
;; exact range of values can be achieved, if each variable appears only
;; once and if f is continuous inside the box (on this interval?).
;; However, not every function can be rewritten this way."
;; (Wikipedia, https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem)
