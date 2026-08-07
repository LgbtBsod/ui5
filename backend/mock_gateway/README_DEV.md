# Backend Development Guide

## Quick Start

```bash
# Install dependencies
pip install -r requirements.txt

# Setup pre-commit hooks
pre-commit install

# Run the server
python -m uvicorn main:app --reload --host 0.0.0.0 --port 8000

# Run tests
pytest tests/ -v

# Run tests with coverage
pytest tests/ --cov=. --cov-report=html
```

## Code Quality

### Pre-commit Hooks

This project uses pre-commit hooks to ensure code quality. Install them with:

```bash
pre-commit install
```

Hooks run automatically on `git commit`:
- Black (code formatting)
- isort (import sorting)
- flake8 (linting)
- mypy (type checking)
- YAML/JSON validation
- Trailing whitespace removal

### Manual Code Quality Checks

```bash
# Format code
black --line-length=120 .
isort --profile=black --line-length=120 .

# Lint
flake8 --max-line-length=120 --ignore=E501,W503,E203,F401 .
pylint api/ services/ utils/ main.py config.py models.py

# Type check
mypy --ignore-missing-imports --no-strict-optional .
```

## Testing

```bash
# Run all tests
pytest tests/ -v

# Run specific test file
pytest tests/test_lock_service_contract.py -v

# Run with coverage
pytest tests/ --cov=. --cov-report=term-missing

# Generate HTML coverage report
pytest tests/ --cov=. --cov-report=html
# Open: htmlcov/index.html
```

## Configuration

Environment variables (see `config.py`):

| Variable | Default | Description |
|----------|---------|-------------|
| `PCCT_DATABASE_URL` | `sqlite:///gateway.db` | Database connection string |
| `PCCT_PROFILE` | `production` | App profile (`production` or `local`) |
| `PCCT_MAX_PAGE_SIZE` | `1000` | Maximum page size for queries |
| `PCCT_MAX_BATCH_OPERATIONS` | `100` | Max operations in $batch request |
| `PCCT_RATE_LIMIT_REQUESTS` | `100` | Rate limit requests per window |
| `PCCT_RATE_LIMIT_WINDOW_SECONDS` | `60` | Rate limit window in seconds |
| `PCCT_ALLOW_MOCK_USER_HEADER` | `false` (local only) | Allow mock user header |
| `PCCT_AUTO_MUTATE_SCHEMA` | `true` (local only) | Auto-migrate schema on startup |
| `PCCT_AUTO_SEED_STARTUP_DATA` | `true` (local only) | Auto-seed test data on startup |
| `PCCT_PROMPT_LOGIN_ON_STARTUP` | `true` | Show login prompt at startup |
| `PCCT_LOG_REQUEST_BODIES` | `false` | Log request bodies |

## Project Structure

```
mock_gateway/
├── api/                    # FastAPI route handlers
│   ├── gateway_*.py       # Gateway API endpoints
│   └── ...
├── services/               # Business logic layer
│   ├── lock_service.py    # Lock management
│   ├── draft_service.py   # Draft handling
│   └── ...
├── repo/                   # Data access layer
│   └── settings_repo.py
├── utils/                  # Utilities & helpers
│   ├── odata*.py          # OData utilities
│   ├── filter_*.py        # OData filter parsing
│   └── ...
├── tests/                  # Test suite
│   ├── conftest.py        # Pytest fixtures
│   └── test_*.py
├── main.py                 # Application entry point
├── config.py               # Configuration
├── models.py               # SQLAlchemy models
├── database.py             # Database setup
├── bootstrap.py            # Schema bootstrap & seeding
├── middleware.py           # Custom middleware
├── background_jobs.py      # Background tasks
├── requirements.txt        # Python dependencies
├── pyproject.toml          # Tool configurations
└── .pre-commit-config.yaml # Pre-commit hooks
```

## Architecture

### Layer Separation

1. **API Layer** (`api/`): FastAPI routers, request/response handling
2. **Service Layer** (`services/`): Business logic, orchestration
3. **Repository Layer** (`repo/`): Data access, queries
4. **Utils Layer** (`utils/`): Shared utilities, OData helpers

### Key Patterns

- **Dependency Injection**: Use FastAPI's `Depends()` for services
- **Session Management**: SQLAlchemy sessions via `get_db()` dependency
- **Error Handling**: Custom exceptions with OData error format
- **Type Hints**: Full type annotations on all public APIs
- **Docstrings**: Google-style docstrings on public functions

## Best Practices

### Code Style

- Line length: 120 characters
- Imports: Sorted with isort (Black profile)
- Formatting: Black automatic formatting
- Types: Full type hints on function signatures

### Function Design

- Max arguments: 7 (use data classes for more)
- Max locals: 25
- Max statements: 60
- Single responsibility principle

### Error Handling

```python
from fastapi import HTTPException
from utils.odata import odata_error_response

@app.get("/resource/{id}")
def get_resource(id: str, db: Session = Depends(get_db)):
    resource = db.query(Resource).filter(Resource.id == id).first()
    if not resource:
        raise HTTPException(
            status_code=404,
            detail=odata_error_response(404, "NOT_FOUND", f"Resource {id} not found")
        )
    return resource
```

### Type Hints

```python
from typing import Any
from fastapi import Response
from sqlalchemy.orm import Session

def auto_save(
    payload: dict[str, Any],
    response: Response,
    db: Session,
    if_match: str | None = None,
) -> dict[str, Any]:
    """Handle AutoSave operation.
    
    Args:
        payload: Request payload containing checklist data
        response: FastAPI response object
        db: Database session
        if_match: ETag for optimistic concurrency
        
    Returns:
        Dictionary with save confirmation and lock status
        
    Raises:
        HTTPException: If lock validation fails
    """
    pass
```

## Monitoring

### Health Checks

- `GET /` - Basic health check
- Background jobs for lock cleanup and metadata refresh

### Logging

Structured logging with configurable request body logging:

```bash
export PCCT_LOG_REQUEST_BODIES=true
```

### Metrics (Optional)

Prometheus metrics available when `prometheus-client` is installed.

## Troubleshooting

### Database Issues

```bash
# Reset database (WARNING: deletes all data)
rm gateway.db
python bootstrap.py
```

### Test Failures

```bash
# Clean pytest cache
rm -rf .pytest_cache __pycache__
pytest tests/ -v --cache-clear
```

### Import Errors

```bash
# Reinstall dependencies
pip install -r requirements.txt --force-reinstall
```

## Contributing

1. Create feature branch
2. Make changes
3. Run tests: `pytest tests/ -v`
4. Check coverage: `pytest tests/ --cov=. --cov-report=term-missing`
5. Run linters: `black . && isort . && flake8 .`
6. Commit (pre-commit hooks will run)
7. Create pull request
