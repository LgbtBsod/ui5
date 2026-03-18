#!/usr/bin/env python3
import argparse
import re
import sys
from pathlib import Path

import requests

CRITICAL_TOKENS = [
    'EntitySet Name="ChecklistRootSet"',
    'EntitySet Name="ChecklistSearchSet"',
    'EntitySet Name="LockStatusSet"',
    'EntitySet Name="AttachmentSet"',
    'FunctionImport Name="SaveChanges"',
    'FunctionImport Name="AutoSave"',
    'FunctionImport Name="LockAcquire"',
    'FunctionImport Name="LockHeartbeat"',
    'FunctionImport Name="LockRelease"',
]


def fetch(url: str) -> str:
    r = requests.get(url, timeout=20)
    r.raise_for_status()
    return r.text


def normalize(xml: str) -> str:
    return re.sub(r"\s+", " ", xml or "").strip()


def main() -> int:
    p = argparse.ArgumentParser(description='Compare mock and real Gateway metadata for critical contract tokens.')
    p.add_argument('--mock', required=True, help='Mock metadata URL')
    p.add_argument('--real', required=True, help='Real metadata URL')
    p.add_argument('--out', default='docs/artifacts/gateway-metadata-drift-report.txt')
    args = p.parse_args()

    mock_xml = fetch(args.mock)
    real_xml = fetch(args.real)

    mock_norm = normalize(mock_xml)
    real_norm = normalize(real_xml)

    missing_in_real = [t for t in CRITICAL_TOKENS if t in mock_norm and t not in real_norm]
    missing_in_mock = [t for t in CRITICAL_TOKENS if t in real_norm and t not in mock_norm]

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    lines = [
        'gateway-metadata-drift-gate',
        f'mock={args.mock}',
        f'real={args.real}',
        f'missing_in_real={len(missing_in_real)}',
        f'missing_in_mock={len(missing_in_mock)}',
    ]
    lines += [f'- missing_in_real: {t}' for t in missing_in_real]
    lines += [f'- missing_in_mock: {t}' for t in missing_in_mock]
    out_path.write_text('\n'.join(lines) + '\n', encoding='utf-8')

    if missing_in_real or missing_in_mock:
        print('gateway-metadata-drift-gate FAIL')
        print('\n'.join(lines))
        return 1

    print('gateway-metadata-drift-gate PASS')
    print(f'report: {out_path}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
