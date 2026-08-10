from __future__ import annotations

from xnormalize_r import normalize_r_source


def test_normalize_expands_chained_assignments_right_to_left() -> None:
    src = "  a <- b <- c <- expensive_call() # once\n"

    assert normalize_r_source(src) == (
        "  c <- expensive_call()\n"
        "  b <- c\n"
        "  a <- b # once\n"
    )


def test_normalize_leaves_nested_assignments_and_assignment_text_unchanged() -> None:
    src = (
        'x <- f(value = "a <- b <- 1")\n'
        "x <- (y <- 1)\n"
        "x <- y + (z <- 2)\n"
    )

    assert normalize_r_source(src) == src
