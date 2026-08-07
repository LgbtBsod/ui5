"""
Dependency Injection Container for Redux Backend.

Provides a centralized way to manage dependencies (State, Config, Services)
and inject them into handlers, improving testability and decoupling.
"""
from typing import Any, Callable, Type, TypeVar, Optional, Dict
from dataclasses import dataclass, field

from redux.state import StateManager
from redux.config import serve_config
from redux.logging_config import get_logger

logger = get_logger(__name__)

T = TypeVar('T')


@dataclass
class ServiceContainer:
    """
    Central registry for application services and configuration.
    
    Usage:
        container = ServiceContainer()
        handler = MyHandler(container)
    """
    state_manager: StateManager = field(default_factory=StateManager)
    config: Any = field(default_factory=lambda: serve_config)
    _overrides: Dict[str, Any] = field(default_factory=dict)

    def get(self, service_type: Type[T]) -> T:
        """
        Resolve a dependency by type.
        
        Args:
            service_type: The class/type of the dependency to resolve.
            
        Returns:
            An instance of the requested service.
        """
        # Check for test overrides first
        if service_type in self._overrides:
            return self._overrides[service_type]
        
        # Default resolutions
        if service_type == StateManager:
            return self.state_manager  # type: ignore
        if service_type == type(serve_config):
            return self.config  # type: ignore
            
        raise ValueError(f"Service {service_type} not registered in container")

    def override(self, service_type: Type[T], instance: T) -> None:
        """
        Override a dependency (useful for testing).
        
        Args:
            service_type: The class/type to override.
            instance: The mock or alternative instance to use.
        """
        self._overrides[service_type] = instance
        logger.debug(f"Overridden service {service_type} with {instance}")

    def reset_overrides(self) -> None:
        """Clear all overrides (call after tests)."""
        self._overrides.clear()
        logger.debug("Service container overrides reset")


# Global default container instance
# In production, this is initialized once at startup.
# In tests, it can be replaced or overridden.
container = ServiceContainer()


def inject(service_type: Type[T]) -> Callable[[Callable[..., Any]], Callable[..., Any]]:
    """
    Decorator to inject dependencies into a function or method.
    
    Usage:
        @inject(StateManager)
        def my_function(state: StateManager):
            ...
            
    Note: For class constructors, it's often cleaner to pass the container
    directly or use explicit constructor injection.
    """
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            # Simple resolution logic could go here if needed
            # For now, we rely on explicit container passing in constructors
            return func(*args, **kwargs)
        return wrapper
    return decorator
