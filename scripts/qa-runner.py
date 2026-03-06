#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import re
import subprocess
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TRACKER_FILE = ROOT / 'docs' / 'artifacts' / 'gitless-change-tracker.json'


def run(cmd):
    try:
        proc = subprocess.run(cmd, cwd=ROOT, text=True, capture_output=True)
        return proc.returncode, (proc.stdout or '') + (proc.stderr or '')
    except FileNotFoundError as exc:
        return 127, str(exc)


def changed_files(base='HEAD'):
    code, out = run(['git', 'diff', '--name-only', base])
    if code == 0:
        return [x.strip() for x in out.splitlines() if x.strip()]
    tracker = load_gitless_tracker()
    current = collect_gitless_fingerprints()
    changed = []
    previous = tracker.get('files', {})
    for file, fp in current.items():
        prev = previous.get(file)
        if prev != fp:
            changed.append(file)
    for file in previous.keys():
        if file not in current:
            changed.append(file)
    save_gitless_tracker(current)
    return sorted(set(changed))


def should_skip_gitless(path_rel):
    normalized = path_rel.replace('\\', '/')
    parts = normalized.split('/')
    if normalized.startswith('docs/artifacts/'):
        return True
    if any(part in {'.git', 'node_modules', 'dist', 'coverage', '__pycache__', '.pytest_cache'} for part in parts):
        return True
    if Path(normalized).suffix.lower() in {'.png', '.jpg', '.jpeg', '.gif', '.svg', '.woff', '.woff2', '.ttf', '.ico', '.db', '.sqlite', '.pyc'}:
        return True
    return False


def load_gitless_tracker():
    if not TRACKER_FILE.exists():
        return {'files': {}, 'generatedAt': None, 'source': 'gitless'}
    try:
        return json.loads(TRACKER_FILE.read_text(encoding='utf-8'))
    except Exception:
        return {'files': {}, 'generatedAt': None, 'source': 'gitless'}


def save_gitless_tracker(files):
    TRACKER_FILE.parent.mkdir(parents=True, exist_ok=True)
    TRACKER_FILE.write_text(json.dumps({
        'generatedAt': datetime.now(timezone.utc).isoformat(),
        'source': 'gitless',
        'files': files
    }, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


def collect_gitless_fingerprints():
    result = {}
    for path in ROOT.rglob('*'):
        if not path.is_file():
            continue
        rel = path.relative_to(ROOT).as_posix()
        if should_skip_gitless(rel):
            continue
        stat = path.stat()
        result[rel] = {
            'size': stat.st_size,
            'mtimeMs': int(stat.st_mtime * 1000),
            'sha1': hashlib.sha1(path.read_bytes()).hexdigest()
        }
    return result


def list_failed_gates(gates):
    return [g for g in gates if g.get('ok') is False]


def summarize_failed_gates(gates):
    return [g['name'] for g in list_failed_gates(gates)]


def tail_lines(text, max_lines):
    lines = text.splitlines()
    return '\n'.join(lines[-max_lines:])


def parse_gate_summary(output):
    gates = []
    current = None
    for line in output.splitlines():
        hit = re.match(r'^\[(\d+)/(\d+)\]\s+(.+)$', line.strip())
        if hit:
            current = {'index': int(hit.group(1)), 'total': int(hit.group(2)), 'name': hit.group(3), 'ok': None}
            gates.append(current)
            continue
        if line.strip() == 'PASS' and current and current['ok'] is None:
            current['ok'] = True
        if line.strip() == 'FAIL' and current and current['ok'] is None:
            current['ok'] = False
    return gates


def parse_validator_map():
    qa_all = ROOT / 'scripts' / 'qa-all.js'
    if not qa_all.exists():
        return {}
    txt = qa_all.read_text(encoding='utf-8')
    result = {}
    re_item = re.compile(r"\{\s*name:\s*'([^']+)'\s*,\s*file:\s*'([^']+)'\s*\}")
    for m in re_item.finditer(txt):
        result[m.group(1)] = m.group(2)
    return result


def gate_doc_path(gate_name):
    slug = gate_name.lower().replace(' ', '-').replace('_', '-')
    candidate = ROOT / 'docs' / 'qa-rules' / f'{slug}.md'
    return str(candidate.relative_to(ROOT)) if candidate.exists() else 'docs/qa-rules/README.md'


def build_errors(output, gates, validator_map):
    if 'FAIL' not in output:
        return []
    failed = list_failed_gates(gates)
    first = failed[0]['name'] if failed else 'qa-all'
    script_file = validator_map.get(first)
    command_hint = f'node scripts/{script_file}' if script_file else f'check scripts/qa-all.js mapping for {first}'
    return [{
        'ruleId': f'QA.{first}',
        'severity': 'BLOCKER',
        'file': f'scripts/{script_file}' if script_file else 'scripts/qa-all.js',
        'message': f'Gate failed: {first}',
        'evidence': output[-2500:],
        'fixHint': f'Run failing gate directly: {command_hint}.',
        'goodExample': 'All gates PASS and pipeline exits 0.',
        'badExample': f'{first} reports FAIL and terminates pipeline.',
        'doc': gate_doc_path(first),
        'suggestedPatch': {
            'path': f'scripts/autofix/out/{first}.patch',
            'unifiedDiff': f'# Suggested fix placeholder for gate {first}\n# Apply manually after reviewing docs/qa-rules and gate output.\n'
        }
    }]


def emit_patch_suggestions(errors):
    out_dir = ROOT / 'scripts' / 'autofix' / 'out'
    out_dir.mkdir(parents=True, exist_ok=True)
    for err in errors:
        patch = err.get('suggestedPatch')
        if not patch:
            continue
        target = ROOT / patch.get('path', '')
        if not str(target).startswith(str(out_dir)):
            continue
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(patch.get('unifiedDiff', ''), encoding='utf-8')


def evaluate_stop_conditions():
    checks = []

    shadow_ok = not any((ROOT / d).exists() for d in ['service/autosave', 'service/search', 'service/detail'])
    checks.append({'name': 'No shadow legacy folders remain', 'ok': shadow_ok})

    dead_report = (ROOT / 'docs' / 'dead-code-report.md')
    dead_ok = dead_report.exists() and '## Unreferenced non-allowlisted runtime modules\n- none' in dead_report.read_text(encoding='utf-8')
    checks.append({'name': 'Dead code report is allowlist-only', 'ok': dead_ok})

    shadow_report = (ROOT / 'docs' / 'shadow-duplicates-report.md')
    shadow_dup_ok = shadow_report.exists() and '## Shadow layer files\n- none' in shadow_report.read_text(encoding='utf-8') and '## Critical basename duplicates\n- none' in shadow_report.read_text(encoding='utf-8')
    checks.append({'name': 'Shadow duplicates report is empty/allowlisted', 'ok': shadow_dup_ok})

    full_report = (ROOT / 'docs' / 'full-audit-report.md')
    stale_ok = full_report.exists() and '## Stale tooling references\n- none' in full_report.read_text(encoding='utf-8')
    checks.append({'name': 'No stale references in scripts/docs', 'ok': stale_ok})

    backend_report = (ROOT / 'docs' / 'backend-layer-report.md')
    backend_ok = backend_report.exists() and 'Canonical low-level client' in backend_report.read_text(encoding='utf-8')
    checks.append({'name': 'Backend abstraction layer reported as canonical', 'ok': backend_ok})

    return checks


def write_report(path, ok, output, gates, changed, stop_checks, trend=None, stability=None, deltas=None, streaks=None, actions=None, health=None):
    lines = [
        '# QA report (latest)', '',
        '## Gate summary', '',
        f'- Status: **{"PASS" if ok else "FAIL"}**',
        f'- Total gates observed: **{len(gates)}**', ''
    ]
    for g in gates:
        icon = '✅' if g.get('ok') else ('❌' if g.get('ok') is False else '⚠️')
        lines.append(f'- {icon} {g["name"]}')

    lines += ['', '## Top offenders', '']
    if ok:
        lines.append('- None.')
    else:
        lines += ['```', output[-4000:], '```']

    lines += [
        '', '## Fix hints', '',
        '- Start with first failed gate from summary and apply rule doc guidance.',
        '- Run the failing gate directly for focused diagnostics.',
        '- Keep changes small and rerun `npm run qa` after each patch set.',
        '', '## Documentation links', '',
        '- docs/qa-rules/README.md',
        '- docs/qa-rules/dead-code.unreferenced-module.md',
        '- docs/qa-rules/drift.forbidden-edge.md'
    ]

    lines += ['', '## Stop conditions', '']
    for c in stop_checks:
        icon = '✅' if c.get('ok') else '❌'
        lines.append(f'- {icon} {c["name"]}')

    if changed:
        js_changed = [f for f in changed if f.endswith('.js')]
        lines += ['', '## Changed JS dependency scan summary', '']
        lines += [f'- {f}' for f in js_changed] if js_changed else ['- No changed JS files.']

    if trend:
        lines += ['', '## Recent QA trend', '']
        lines.append(f'- Window: **{trend.get("runs", 0)}** run(s) (configured: {trend.get("window", 0)})')
        lines.append(f'- Pass rate: **{round(float(trend.get("passRate", 0.0)) * 100, 1)}%**')
        lines.append(f'- Failing runs: **{trend.get("failures", 0)}**')
        top_failed = trend.get('topFailedGates') or []
        if top_failed:
            lines.append('- Most frequent failing gates:')
            for row in top_failed:
                lines.append(f'  - {row.get("name")}: {row.get("count")}')
        else:
            lines.append('- Most frequent failing gates: none.')

    if stability:
        lines += ['', '## Gate stability alerts', '']
        alerts = stability.get('alerts') or []
        if alerts:
            lines.append('| Gate | Fail rate | Failures/Runs | Flakiness |')
            lines.append('|---|---:|---:|---:|')
            for row in alerts:
                lines.append(
                    f"| {row.get('name')} | {round(float(row.get('failRate', 0.0)) * 100, 1)}% | {row.get('failures', 0)}/{row.get('runs', 0)} | {round(float(row.get('flakiness', 0.0)) * 100, 1)}% |"
                )
        else:
            lines.append('- No unstable gates detected in configured window.')

    if deltas and deltas.get('available'):
        lines += ['', '## Run-to-run gate delta', '']
        lines.append(f"- Total failed gates delta: **{deltas.get('failuresDelta', 0):+d}**")
        if deltas.get('passToFail'):
            lines.append('- Status transition: ⚠️ PASS → FAIL')
        if deltas.get('failToPass'):
            lines.append('- Status transition: ✅ FAIL → PASS')
        new_failures = deltas.get('newFailures') or []
        resolved = deltas.get('resolvedFailures') or []
        lines.append('- New failing gates: ' + (', '.join(new_failures) if new_failures else 'none'))
        lines.append('- Resolved failing gates: ' + (', '.join(resolved) if resolved else 'none'))

    if streaks:
        lines += ['', '## Gate streaks', '']
        failing = streaks.get('failing') or []
        passing = streaks.get('passing') or []
        lines.append('- Current failing streaks:')
        if failing:
            for row in failing:
                lines.append(f"  - {row.get('name')}: {row.get('streak')} run(s)")
        else:
            lines.append('  - none')

        lines.append('- Current passing streaks (previously failing gates):')
        if passing:
            for row in passing:
                lines.append(f"  - {row.get('name')}: {row.get('streak')} run(s)")
        else:
            lines.append('  - none')

    if actions:
        lines += ['', '## Recommended next actions', '']
        for idx, action in enumerate(actions, start=1):
            lines.append(f'{idx}. {action}')

    if health:
        lines += ['', '## QA health score', '']
        lines.append(f"- Score: **{health.get('score', 0)} / 100** ({health.get('band', 'watch')})")
        lines.append(f"- Pass rate: **{round(float(health.get('passRate', 0.0)) * 100, 1)}%**")
        lines.append(f"- Unstable gates: **{health.get('unstableGateCount', 0)}**")
        lines.append(f"- New failures (latest run): **{health.get('newFailureCount', 0)}**")
        lines.append(f"- Max failing streak: **{health.get('maxFailingStreak', 0)}**")

    Path(path).parent.mkdir(parents=True, exist_ok=True)
    Path(path).write_text('\n'.join(lines) + '\n', encoding='utf-8')


def append_cycle_log(path, ok, output, gates, dedupe=True, tail_max_lines=30, include_raw=False):
    ts = datetime.now(timezone.utc).isoformat()
    failed = [g['name'] for g in gates if g.get('ok') is False]
    digest = hashlib.sha1(output.encode('utf-8')).hexdigest()[:12]
    prev = last_cycle_digest(path)
    if dedupe and prev == digest:
        return

    body = [
        f'\n## Cycle {ts}',
        f'- QA status: **{"PASS" if ok else "FAIL"}**',
        f'- Gates observed: {len(gates)}',
        f'- Failed gates: {", ".join(failed) if failed else "none"}',
        f'- Output digest: `{digest}`',
    ]
    if include_raw:
        body += [
            '- Raw QA output (tail):',
            '```',
            tail_lines(output.strip(), tail_max_lines),
            '```',
        ]
    p = Path(path)
    if not p.exists():
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text('# Cycle log\n', encoding='utf-8')
    with p.open('a', encoding='utf-8') as f:
        f.write('\n'.join(body) + '\n')


def emit_architecture_artifacts(enabled):
    if not enabled:
        return
    run(['node', 'scripts/generate-architecture-audit.js'])


def last_cycle_digest(path):
    p = Path(path)
    if not p.exists():
        return None
    txt = p.read_text(encoding='utf-8')
    m = re.findall(r'- Output digest: `([0-9a-f]{12})`', txt)
    return m[-1] if m else None


def load_history(path):
    p = Path(path)
    if not p.exists():
        return []
    try:
        raw = json.loads(p.read_text(encoding='utf-8'))
    except json.JSONDecodeError:
        return []
    return raw if isinstance(raw, list) else []


def save_history(path, history):
    p = Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(history, indent=2) + '\n', encoding='utf-8')


def build_history_entry(ok, gates, output):
    failed = [g['name'] for g in gates if g.get('ok') is False]
    return {
        'timestamp': datetime.now(timezone.utc).isoformat(),
        'ok': ok,
        'gatesSeen': len(gates),
        'failedGates': failed,
        'outputDigest': hashlib.sha1(output.encode('utf-8')).hexdigest()[:12]
    }


def update_history(path, ok, gates, output, limit=120):
    history = load_history(path)
    history.append(build_history_entry(ok, gates, output))
    history = history[-max(1, limit):]
    save_history(path, history)
    return history


def summarize_recent_trend(history, window=20):
    items = history[-max(1, window):]
    if not items:
        return {
            'window': window,
            'runs': 0,
            'passRate': 0.0,
            'failures': 0,
            'topFailedGates': []
        }

    failures = [x for x in items if not x.get('ok')]
    gate_counts = {}
    for row in failures:
        for gate in row.get('failedGates', []):
            gate_counts[gate] = gate_counts.get(gate, 0) + 1

    top_failed = sorted(gate_counts.items(), key=lambda kv: (-kv[1], kv[0]))[:5]
    pass_rate = round((len(items) - len(failures)) / len(items), 4)

    return {
        'window': window,
        'runs': len(items),
        'passRate': pass_rate,
        'failures': len(failures),
        'topFailedGates': [{'name': name, 'count': count} for name, count in top_failed]
    }


def summarize_gate_stability(history, window=30, min_runs=8, fail_rate_threshold=0.35):
    items = history[-max(1, window):]
    if not items:
        return {
            'window': window,
            'runs': 0,
            'alerts': []
        }

    gate_names = set()
    for row in items:
        for gate in row.get('failedGates', []):
            gate_names.add(gate)

    alerts = []
    for gate in sorted(gate_names):
        series = [gate in (row.get('failedGates') or []) for row in items]
        failures = sum(1 for x in series if x)
        fail_rate = failures / len(series)
        transitions = sum(1 for i in range(1, len(series)) if series[i] != series[i - 1])
        flakiness = transitions / max(1, len(series) - 1)

        if len(series) >= min_runs and fail_rate >= fail_rate_threshold:
            alerts.append({
                'name': gate,
                'runs': len(series),
                'failures': failures,
                'failRate': round(fail_rate, 4),
                'flakiness': round(flakiness, 4)
            })

    alerts = sorted(alerts, key=lambda x: (-x['failRate'], -x['flakiness'], x['name']))[:7]
    return {
        'window': window,
        'runs': len(items),
        'alerts': alerts
    }


def summarize_gate_deltas(history):
    if len(history) < 2:
        return {
            'available': False,
            'newFailures': [],
            'resolvedFailures': [],
            'failuresDelta': 0,
            'passToFail': False,
            'failToPass': False
        }

    prev = history[-2]
    current = history[-1]
    prev_failed = set(prev.get('failedGates') or [])
    curr_failed = set(current.get('failedGates') or [])

    return {
        'available': True,
        'newFailures': sorted(curr_failed - prev_failed),
        'resolvedFailures': sorted(prev_failed - curr_failed),
        'failuresDelta': len(curr_failed) - len(prev_failed),
        'passToFail': bool(prev.get('ok')) and not bool(current.get('ok')),
        'failToPass': (not bool(prev.get('ok'))) and bool(current.get('ok'))
    }


def summarize_gate_streaks(history, max_items=10):
    if not history:
        return {
            'failing': [],
            'passing': []
        }

    gate_names = set()
    for row in history:
        for gate in row.get('failedGates', []):
            gate_names.add(gate)

    failing = []
    passing = []
    total_runs = len(history)

    for gate in sorted(gate_names):
        fail_streak = 0
        for row in reversed(history):
            if gate in (row.get('failedGates') or []):
                fail_streak += 1
            else:
                break

        pass_streak = 0
        for row in reversed(history):
            if gate in (row.get('failedGates') or []):
                break
            pass_streak += 1

        if fail_streak > 0:
            failing.append({'name': gate, 'streak': fail_streak})
        if pass_streak > 0 and pass_streak < total_runs:
            passing.append({'name': gate, 'streak': pass_streak})

    failing = sorted(failing, key=lambda x: (-x['streak'], x['name']))[:max_items]
    passing = sorted(passing, key=lambda x: (-x['streak'], x['name']))[:max_items]
    return {'failing': failing, 'passing': passing}


def build_priority_actions(trend=None, stability=None, deltas=None, streaks=None):
    actions = []

    if deltas and deltas.get('newFailures'):
        actions.append(f"Investigate newly failing gates first: {', '.join(deltas.get('newFailures')[:3])}.")

    unstable = (stability or {}).get('alerts') or []
    if unstable:
        top = unstable[0]
        actions.append(
            f"Reduce instability in {top.get('name')} (fail-rate {round(float(top.get('failRate', 0.0)) * 100, 1)}%, flakiness {round(float(top.get('flakiness', 0.0)) * 100, 1)}%)."
        )

    failing_streaks = (streaks or {}).get('failing') or []
    if failing_streaks:
        top = failing_streaks[0]
        actions.append(f"Address persistent failure streak: {top.get('name')} failing {top.get('streak')} run(s) in a row.")

    if not actions and trend and trend.get('runs', 0) > 0 and float(trend.get('passRate', 0.0)) < 0.95:
        actions.append('Raise pass-rate above 95% by prioritizing top frequent failing gates.')

    return actions[:5]


def build_health_summary(trend=None, stability=None, deltas=None, streaks=None):
    pass_rate = float((trend or {}).get('passRate', 0.0))
    unstable_count = len((stability or {}).get('alerts') or [])
    new_failures_count = len((deltas or {}).get('newFailures') or [])
    max_failing_streak = max([x.get('streak', 0) for x in ((streaks or {}).get('failing') or [])], default=0)

    score = 100.0
    score -= (1.0 - pass_rate) * 55.0
    score -= min(unstable_count, 6) * 5.0
    score -= min(new_failures_count, 6) * 6.0
    score -= min(max_failing_streak, 10) * 2.0
    score = max(0.0, min(100.0, score))

    if score >= 90:
        band = 'excellent'
    elif score >= 75:
        band = 'good'
    elif score >= 55:
        band = 'watch'
    else:
        band = 'critical'

    return {
        'score': round(score, 1),
        'band': band,
        'passRate': round(pass_rate, 4),
        'unstableGateCount': unstable_count,
        'newFailureCount': new_failures_count,
        'maxFailingStreak': max_failing_streak
    }


def run_pipeline(failfast=False):
    if failfast:
        return run(['npm', 'run', 'qa'])
    return run(['node', 'scripts/qa-all.js'])


def write_json_payload(path, payload):
    if not path:
        return
    p = Path(path)
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(payload, indent=2) + '\n', encoding='utf-8')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--changed', action='store_true')
    parser.add_argument('--changed-base', default='HEAD')
    parser.add_argument('--json', action='store_true')
    parser.add_argument('--json-file')
    parser.add_argument('--report', default='docs/qa-report-latest.md')
    parser.add_argument('--failfast', action='store_true')
    parser.add_argument('--cycle-log', default='docs/cycle-log.md')
    parser.add_argument('--no-cycle-log', action='store_true')
    parser.add_argument('--emit-artifacts', action='store_true')
    parser.add_argument('--no-cycle-dedupe', action='store_true')
    parser.add_argument('--cycle-tail-lines', type=int, default=30)
    parser.add_argument('--cycle-log-raw', action='store_true')
    parser.add_argument('--history-file', default='docs/qa-history-latest.json')
    parser.add_argument('--history-limit', type=int, default=120)
    parser.add_argument('--trend-window', type=int, default=20)
    parser.add_argument('--stability-window', type=int, default=30)
    parser.add_argument('--stability-min-runs', type=int, default=8)
    parser.add_argument('--stability-fail-rate-threshold', type=float, default=0.35)
    args = parser.parse_args()

    emit_architecture_artifacts(args.emit_artifacts)
    code, out = run_pipeline(failfast=args.failfast)
    ok = code == 0
    gates = parse_gate_summary(out)
    changed = changed_files(args.changed_base) if args.changed else []
    validator_map = parse_validator_map()
    errors = build_errors(out, gates, validator_map)
    stop_checks = evaluate_stop_conditions()
    history = update_history(args.history_file, ok, gates, out, limit=args.history_limit)
    trend = summarize_recent_trend(history, window=args.trend_window)
    stability = summarize_gate_stability(
        history,
        window=args.stability_window,
        min_runs=args.stability_min_runs,
        fail_rate_threshold=args.stability_fail_rate_threshold
    )
    deltas = summarize_gate_deltas(history)
    streaks = summarize_gate_streaks(history)
    actions = build_priority_actions(trend=trend, stability=stability, deltas=deltas, streaks=streaks)
    health = build_health_summary(trend=trend, stability=stability, deltas=deltas, streaks=streaks)
    write_report(
        args.report,
        ok,
        out,
        gates,
        changed,
        stop_checks,
        trend=trend,
        stability=stability,
        deltas=deltas,
        streaks=streaks,
        actions=actions,
        health=health
    )
    if not args.no_cycle_log:
        append_cycle_log(
            args.cycle_log,
            ok,
            out,
            gates,
            dedupe=not args.no_cycle_dedupe,
            tail_max_lines=args.cycle_tail_lines,
            include_raw=args.cycle_log_raw
        )
    emit_patch_suggestions(errors)

    payload = {
        'name': 'qa-all',
        'ok': ok,
        'errors': errors,
        'stats': {
            'changedFiles': len(changed),
            'gatesSeen': len(gates),
            'failedGates': len(list_failed_gates(gates)),
            'stopConditionsPassed': len([c for c in stop_checks if c.get('ok')]),
            'stopConditionsTotal': len(stop_checks)
        },
        'meta': {
            'changedBase': args.changed_base if args.changed else None,
            'failedGateNames': summarize_failed_gates(gates),
            'trend': trend,
            'stability': stability,
            'deltas': deltas,
            'streaks': streaks,
            'actions': actions,
            'health': health
        }
    }
    write_json_payload(args.json_file, payload)
    if args.json:
        print(json.dumps(payload, indent=2))
    else:
        print('PASS qa-runner.py' if ok else 'FAIL qa-runner.py')
    raise SystemExit(0 if ok else 1)


if __name__ == '__main__':
    main()
