# Rust Web Development Best Practices

This document outlines the best practices for Rust web development that are incorporated into our multi-agent system.

## Functional Programming Paradigm

### Pure Functions

```rust
// Good: Pure function with no side effects
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Avoid: Function with side effects outside of appropriate context
fn add_with_log(x: i32, y: i32) -> i32 {
    println!("Adding numbers"); // Side effect
    x + y
}
```

**Best Practice**: Keep functions pure whenever possible. Isolate side effects in appropriate monads (IO, State, etc.).

### Immutable Data

```rust
// Good: Using immutable data structures
#[derive(Clone)]
struct User {
    name: String,
}

fn update_user(user: &User, new_name: &str) -> User {
    User {
        name: new_name.to_string(),
        ..user.clone()
    }
}

// Avoid: Attempting to mutate data in-place without &mut or interior mutability
```

**Best Practice**: Design with immutability in mind. Use record updates or lenses for working with complex data structures.

## Type System

### Strong Static Typing

```rust
// Good: Leveraging the type system for safety
enum UserRole {
    Admin,
    Editor,
    Viewer,
}

fn can_edit(role: UserRole) -> bool {
    match role {
        UserRole::Admin | UserRole::Editor => true,
        UserRole::Viewer => false,
    }
}

// Avoid: Using strings or integers for enumerations
fn bad_can_edit(role: &str) -> bool {
    match role {
        "admin" | "editor" => true,
        _ => false,
    }
}
```

**Best Practice**: Use the type system to prevent errors at compile time rather than runtime.

### Algebraic Data Types

```rust
// Good: Using enums to model domain concepts
enum PaymentMethod {
    CreditCard(CreditCardInfo),
    PayPal(PayPalInfo),
    BankTransfer(BankInfo),
}

fn process_payment(method: PaymentMethod) -> PaymentResult {
    match method {
        PaymentMethod::CreditCard(info) => process_credit_card(info),
        PaymentMethod::PayPal(info) => process_paypal(info),
        PaymentMethod::BankTransfer(info) => process_bank_transfer(info),
    }
}
```

**Best Practice**: Use sum types (variants) and product types (records) to model your domain accurately.

### Type Classes

```rust
// Good: Using traits for polymorphism
trait Serializable {
    fn serialize(&self) -> Vec<u8>;
    fn deserialize(bytes: &[u8]) -> Option<Self> where Self: Sized;
}

impl Serializable for User {
    fn serialize(&self) -> Vec<u8> {
        serde_json::to_vec(self).unwrap()
    }
    fn deserialize(bytes: &[u8]) -> Option<Self> {
        serde_json::from_slice(bytes).ok()
    }
}
```

**Best Practice**: Use type classes to define behavior that can be implemented by multiple types.

## Error Handling

### Maybe and Either

```rust
// Good: Using Option for operations that might fail
fn find_user(user_id: UserId) -> Option<User> {
    // ...
}

// Good: Using Result for operations that might fail with an error message
fn validate_user(data: UserData) -> Result<User, ValidationError> {
    // ...
}
```

**Best Practice**: Use Maybe for operations that might not return a value, and Either for operations that might fail with an error.

### ExceptT

```rust
// Good: Using Result and error propagation for combining IO and error handling
fn import_user(path: &str) -> Result<User, ImportError> {
    let content = std::fs::read_to_string(path).map_err(ImportError::FileReadError)?;
    parse_user(&content).ok_or(ImportError::InvalidFormat)
}
```

**Best Practice**: Use monad transformers like ExceptT to combine effects like IO with error handling.

## Web Framework Patterns

### Handler Pattern

```rust
// Good: Using the handler pattern with Actix-web or Axum
use axum::{routing::get, Router, extract::Path};

async fn get_all_users() -> impl axum::response::IntoResponse {
    // ...
}

async fn get_user_by_id(Path(user_id): Path<UserId>) -> impl axum::response::IntoResponse {
    // ...
}

let app = Router::new()
    .route("/users", get(get_all_users))
    .route("/users/:user_id", get(get_user_by_id));
```

**Best Practice**: Organize API endpoints using the handler pattern, separating routing from implementation.

### Repository Pattern

```rust
// Good: Using the repository pattern to abstract database access
trait UserRepository {
    fn get_user(&self, user_id: UserId) -> Option<User>;
    fn create_user(&self, user: &User) -> UserId;
    fn update_user(&self, user: &User) -> bool;
    fn delete_user(&self, user_id: UserId) -> bool;
}

// PostgreSQL implementation would implement this trait
```

instance UserRepository PgM where
  getUser userId = runQuery $ selectUser userId
  createUser user = runQuery $ insertUser user
  updateUser user = runQuery $ updateUserQuery user
  deleteUser userId = runQuery $ deleteUserQuery userId

```

**Best Practice**: Use the repository pattern to abstract database access, making it easier to test and change implementations.

## Performance Optimization

### Lazy Evaluation

```rust
-- Good: Leveraging lazy evaluation for efficiency
findFirstMatch :: (a -> Bool) -> [a] -> Maybe a
findFirstMatch predicate xs = listToMaybe $ filter predicate xs
```

**Best Practice**: Understand and leverage lazy evaluation, but be aware of potential space leaks.

### Strict Evaluation When Needed

```rust
-- Good: Using strict evaluation when appropriate
import Data.Text (Text)
import qualified Data.Text as T

processLargeText :: Text -> Text
processLargeText text = T.foldl' accumulator "" text
  where
    accumulator acc char = acc <> T.singleton (transform char)
    transform c = ...
```

**Best Practice**: Use strict data types (Text, ByteString) and strict operations (foldl') when working with large data.

## Testing

### Property-Based Testing

```rust
// Good: Using proptest or quickcheck for property-based testing
use proptest::prelude::*;

proptest! {
    #[test]
    fn reverse_involutive(xs: Vec<i32>) {
        let rev: Vec<i32> = xs.clone().into_iter().rev().collect();
        let rev_rev: Vec<i32> = rev.into_iter().rev().collect();
        prop_assert_eq!(xs, rev_rev);
    }

    #[test]
    fn sorted_is_ordered(xs: Vec<i32>) {
        let mut sorted = xs.clone();
        sorted.sort();
        prop_assert!(sorted.windows(2).all(|w| w[0] <= w[1]));
    }
}
```

**Best Practice**: Use property-based testing to verify that your functions satisfy important properties across many inputs.

### Unit Testing

```rust
// Good: Using Rust's built-in test framework
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_empty_usernames() {
        assert_eq!(validate_user(User { name: "".into(), email: "email@example.com".into() }), Err(ValidationError::EmptyUsername));
    }

    #[test]
    fn rejects_invalid_email_formats() {
        assert_eq!(validate_user(User { name: "john".into(), email: "not-an-email".into() }), Err(ValidationError::InvalidEmail));
    }

    #[test]
    fn accepts_valid_users() {
        assert_eq!(validate_user(User { name: "john".into(), email: "john@example.com".into() }), Ok(User { name: "john".into(), email: "john@example.com".into() }));
    }
}
```

**Best Practice**: Use unit tests to verify specific behaviors and edge cases.

## Concurrency

### Software Transactional Memory (STM)

```rust
// Good: Using Arc<Mutex<>> for safe concurrent state
use std::sync::{Arc, Mutex};

struct ChatRoom {
    users: Arc<Mutex<Vec<User>>>,
    messages: Arc<Mutex<Vec<Message>>>,
}

impl ChatRoom {
    fn add_user(&self, user: User) {
        let mut users = self.users.lock().unwrap();
        users.push(user);
    }
    fn add_message(&self, msg: Message) {
        let mut messages = self.messages.lock().unwrap();
        messages.push(msg);
    }
    fn add_user_and_broadcast(&self, user: User, msg: Message) {
        self.add_user(user);
        self.add_message(msg);
    }
}
```

**Best Practice**: Use STM for managing shared state in concurrent applications.

### Async

```rust
// Good: Using async for concurrent operations
use futures::future::join_all;

async fn import_users(paths: Vec<String>) -> Vec<Result<User, ImportError>> {
    let futures = paths.into_iter().map(import_user);
    join_all(futures).await
}
```

**Best Practice**: Use the async package for concurrent operations, especially for IO-bound tasks.

## Deployment

### Docker Containerization

```dockerfile
# Good: Multi-stage build for smaller images
FROM rust:1.76 as builder
WORKDIR /app
COPY . .
RUN cargo build --release

FROM debian:bullseye-slim
WORKDIR /app
COPY --from=builder /app/target/release/myapp .
EXPOSE 8080
CMD ["./myapp"]
```

**Best Practice**: Use multi-stage Docker builds to create smaller production images.

### Configuration Management

```rust
// Good: Using environment variables for configuration
use std::env;

struct AppConfig {
    db_host: String,
    db_port: u16,
    db_user: String,
    db_pass: String,
    server_port: u16,
}

impl AppConfig {
    fn load() -> Self {
        AppConfig {
            db_host: env::var("DB_HOST").unwrap_or_else(|_| "localhost".to_string()),
            db_port: env::var("DB_PORT").ok().and_then(|s| s.parse().ok()).unwrap_or(5432),
            db_user: env::var("DB_USER").unwrap_or_else(|_| "postgres".to_string()),
            db_pass: env::var("DB_PASS").unwrap_or_default(),
            server_port: env::var("SERVER_PORT").ok().and_then(|s| s.parse().ok()).unwrap_or(8080),
        }
    }
}
```

**Best Practice**: Use environment variables for configuration, following the 12-factor app methodology.

## Documentation

### Haddock

```rust
/// Validates a user's credentials and returns a JWT token if valid.
///
/// Returns `None` if the credentials are invalid.
///
/// # Examples
///
/// ```
/// let creds = Credentials::new("user", "pass");
/// let token = validate_credentials(&creds);
/// assert!(token.is_some());
/// ```
fn validate_credentials(creds: &Credentials) -> Option<Token> {
    // ...
}
```

**Best Practice**: Use Haddock to document your code, including examples and property descriptions.

## Conclusion

These best practices are incorporated into our multi-agent system for Rust web development. Each agent is specialized in applying these practices in their respective domains:

- **Requirements Analyzer**: Identifies opportunities to leverage Rust's strengths
- **System Architect**: Designs architectures that follow functional programming principles
- **Rust Developer**: Implements code following these best practices
- **QA Engineer**: Tests using property-based and unit testing approaches
- **DevOps Engineer**: Sets up deployment following containerization and configuration best practices
