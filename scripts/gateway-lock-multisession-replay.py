#!/usr/bin/env python3
import argparse
import sys
import requests


def fail(msg: str) -> int:
    print('gateway-lock-multisession-replay FAIL')
    print(msg)
    return 1


def main() -> int:
    p = argparse.ArgumentParser(description='Replay lock semantics with two sessions against a gateway endpoint.')
    p.add_argument('service_root', help='Service root URL, e.g. http://host/sap/opu/odata/sap/Z_EHS...')
    p.add_argument('root_id', help='Checklist root key')
    args = p.parse_args()

    base = args.service_root.rstrip('/')
    root = args.root_id

    s = requests.Session()
    token_resp = s.get(f'{base}/', headers={'X-CSRF-Token': 'Fetch'}, timeout=20)
    token = token_resp.headers.get('X-CSRF-Token', '')
    if not token:
        return fail('missing CSRF token')

    def post(name: str, session_guid: str):
        return s.post(f'{base}/{name}?RootId={root}&SessionGuid={session_guid}', headers={'X-CSRF-Token': token}, timeout=20)

    acq_a = post('LockAcquire', 'A')
    if acq_a.status_code != 200:
        return fail(f'LockAcquire A status={acq_a.status_code}')

    acq_b = post('LockAcquire', 'B')
    if acq_b.status_code not in (200, 409):
        return fail(f'LockAcquire B unexpected status={acq_b.status_code}')

    hb_a = post('LockHeartbeat', 'A')
    if hb_a.status_code != 200:
        return fail(f'LockHeartbeat A status={hb_a.status_code}')

    rel_a = post('LockRelease', 'A')
    if rel_a.status_code != 200:
        return fail(f'LockRelease A status={rel_a.status_code}')

    acq_b2 = post('LockAcquire', 'B')
    if acq_b2.status_code != 200:
        return fail(f'LockAcquire B after release status={acq_b2.status_code}')

    print('gateway-lock-multisession-replay PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
