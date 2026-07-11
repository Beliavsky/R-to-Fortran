from __future__ import annotations

from datetime import datetime

from _pytest.terminal import TerminalReporter, format_session_duration


def _xr2f_summary_time_range(start: datetime, end: datetime) -> str:
    if start.date() == end.date():
        return f"{start:%Y-%m-%d %H:%M:%S}-{end:%H:%M:%S}"
    return f"{start:%Y-%m-%d %H:%M:%S} - {end:%Y-%m-%d %H:%M:%S}"


def pytest_sessionstart(session) -> None:
    session.config._xr2f_wall_start = datetime.now()


def pytest_configure(config) -> None:
    if getattr(TerminalReporter, "_xr2f_summary_stats_patched", False):
        return

    def summary_stats(self: TerminalReporter) -> None:
        if self.verbosity < -1:
            return

        session_duration = self._session_start.elapsed()
        parts, main_color = self.build_summary_stats_line()
        line_parts = []

        display_sep = self.verbosity >= 0
        if display_sep:
            fullwidth = self._tw.fullwidth
        for text, markup in parts:
            with_markup = self._tw.markup(text, **markup)
            if display_sep:
                fullwidth += len(with_markup) - len(text)
            line_parts.append(with_markup)
        msg = ", ".join(line_parts)

        main_markup = {main_color: True}
        duration = f" in {format_session_duration(session_duration.seconds)}"
        duration_with_markup = self._tw.markup(duration, **main_markup)
        if display_sep:
            fullwidth += len(duration_with_markup) - len(duration)
        msg += duration_with_markup

        start = getattr(self.config, "_xr2f_wall_start", None)
        if start is not None:
            time_range = "; " + _xr2f_summary_time_range(start, datetime.now())
            time_range_with_markup = self._tw.markup(time_range, **main_markup)
            if display_sep:
                fullwidth += len(time_range_with_markup) - len(time_range)
            msg += time_range_with_markup

        if display_sep:
            markup_for_end_sep = self._tw.markup("", **main_markup)
            if markup_for_end_sep.endswith("\x1b[0m"):
                markup_for_end_sep = markup_for_end_sep[:-4]
            fullwidth += len(markup_for_end_sep)
            msg += markup_for_end_sep

        if display_sep:
            self.write_sep("=", msg, fullwidth=fullwidth, **main_markup)
        else:
            self.write_line(msg, **main_markup)

    TerminalReporter.summary_stats = summary_stats
    TerminalReporter._xr2f_summary_stats_patched = True
