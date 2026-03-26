#!/usr/bin/env python3
import argparse
import base64
import sys
import requests


def fail(msg: str) -> int:
    print('gateway-attachment-lifecycle-parity FAIL')
    print(msg)
    return 1


def main() -> int:
    p = argparse.ArgumentParser(description='Verify attachment upload/delete lifecycle via canonical OData endpoints.')
    p.add_argument('service_root', help='Service root URL')
    p.add_argument('root_id', help='Checklist root key')
    args = p.parse_args()

    base = args.service_root.rstrip('/')
    root = args.root_id

    s = requests.Session()
    token_resp = s.get(f'{base}/', headers={'X-CSRF-Token': 'Fetch'}, timeout=20)
    token = token_resp.headers.get('X-CSRF-Token', '')
    if not token:
        return fail('missing CSRF token')

    payload = {
        'PARENT_KEY': root,
        'FileName': 'parity.txt',
        'MimeType': 'text/plain',
        'Value': base64.b64encode(b'parity').decode('ascii')
    }
    up = s.post(f'{base}/AttachmentSet', json=payload, headers={'X-CSRF-Token': token}, timeout=20)
    if up.status_code not in (200, 201):
        return fail(f'upload status={up.status_code} body={up.text[:200]}')

    key = ((up.json().get('d') or {}).get('AttachmentKey') or '').strip()
    if not key:
        return fail('upload response missing AttachmentKey')

    dele = s.delete(f"{base}/AttachmentSet('{key}')", headers={'X-CSRF-Token': token}, timeout=20)
    if dele.status_code not in (200, 204):
        return fail(f'delete status={dele.status_code} body={dele.text[:200]}')

    print('gateway-attachment-lifecycle-parity PASS')
    return 0


if __name__ == '__main__':
    sys.exit(main())
