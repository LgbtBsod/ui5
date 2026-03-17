import importlib.util
from pathlib import Path


SPEC = importlib.util.spec_from_file_location("qa_runner", Path("scripts/qa-runner.py"))
qa_runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(qa_runner)


def test_summarize_recent_trend_counts_failures_and_top_gates():
    history = [
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-a", "gate-b"]},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": True, "failedGates": []},
    ]

    trend = qa_runner.summarize_recent_trend(history, window=10)

    assert trend["runs"] == 4
    assert trend["failures"] == 2
    assert trend["passRate"] == 0.5
    assert trend["topFailedGates"][0] == {"name": "gate-a", "count": 2}


def test_summarize_gate_stability_flags_high_fail_rate_gates():
    history = [
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": False, "failedGates": ["gate-a", "gate-b"]},
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-b"]},
    ]

    stability = qa_runner.summarize_gate_stability(
        history,
        window=9,
        min_runs=8,
        fail_rate_threshold=0.4,
    )

    names = [x["name"] for x in stability["alerts"]]
    assert "gate-a" in names
    assert "gate-b" not in names

    gate_a = next(x for x in stability["alerts"] if x["name"] == "gate-a")
    assert gate_a["failures"] == 5
    assert gate_a["runs"] == 9
    assert gate_a["failRate"] == round(5 / 9, 4)


def test_summarize_gate_deltas_detects_new_and_resolved_failures():
    history = [
        {"ok": False, "failedGates": ["gate-a", "gate-b"]},
        {"ok": False, "failedGates": ["gate-b", "gate-c"]},
    ]

    deltas = qa_runner.summarize_gate_deltas(history)

    assert deltas["available"] is True
    assert deltas["newFailures"] == ["gate-c"]
    assert deltas["resolvedFailures"] == ["gate-a"]
    assert deltas["failuresDelta"] == 0
    assert deltas["passToFail"] is False
    assert deltas["failToPass"] is False


def test_summarize_gate_deltas_detects_status_transition_pass_to_fail():
    history = [
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-a"]},
    ]

    deltas = qa_runner.summarize_gate_deltas(history)

    assert deltas["available"] is True
    assert deltas["passToFail"] is True
    assert deltas["failToPass"] is False
    assert deltas["failuresDelta"] == 1


def test_summarize_gate_streaks_reports_current_failing_and_passing_streaks():
    history = [
        {"ok": False, "failedGates": ["gate-a", "gate-b"]},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": True, "failedGates": []},
        {"ok": False, "failedGates": ["gate-a"]},
        {"ok": False, "failedGates": ["gate-a"]},
    ]

    streaks = qa_runner.summarize_gate_streaks(history)

    assert streaks["failing"][0] == {"name": "gate-a", "streak": 2}
    assert {"name": "gate-b", "streak": 4} in streaks["passing"]


def test_summarize_gate_streaks_empty_history():
    streaks = qa_runner.summarize_gate_streaks([])
    assert streaks == {"failing": [], "passing": []}


def test_build_priority_actions_prefers_new_failures_and_unstable_gate():
    actions = qa_runner.build_priority_actions(
        trend={"runs": 12, "passRate": 0.8},
        stability={"alerts": [{"name": "gate-flaky", "failRate": 0.51, "flakiness": 0.42}]},
        deltas={"newFailures": ["gate-new"], "available": True},
        streaks={"failing": [{"name": "gate-streak", "streak": 4}], "passing": []},
    )

    assert actions
    assert "gate-new" in actions[0]
    assert any("gate-flaky" in x for x in actions)
    assert any("gate-streak" in x for x in actions)


def test_build_health_summary_returns_watch_or_critical_for_poor_quality():
    health = qa_runner.build_health_summary(
        trend={"passRate": 0.62},
        stability={"alerts": [{"name": "g1"}, {"name": "g2"}]},
        deltas={"newFailures": ["g3", "g4"]},
        streaks={"failing": [{"name": "g1", "streak": 5}]},
    )

    assert 0 <= health["score"] <= 100
    assert health["band"] in {"watch", "critical"}
    assert health["unstableGateCount"] == 2
    assert health["newFailureCount"] == 2
    assert health["maxFailingStreak"] == 5
