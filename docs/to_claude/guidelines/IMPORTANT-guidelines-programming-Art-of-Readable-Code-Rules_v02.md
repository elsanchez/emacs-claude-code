<!-- ---
-- Timestamp: 2025-05-21
-- Author: Claude
-- File: /home/ywatanabe/.dotfiles/.claude/to_claude/guidelines/IMPORTANT-guidelines-programming-Art-of-Readable-Code-Rules.md
-- --- -->

The Art of Readable Code focuses on creating code that minimizes the time and effort needed for others to understand it. Code readability directly impacts maintainability, bug reduction, and team productivity.
_____________________________________

## Core Philosophy
1. Code should be easy to understand for humans.
2. Readability is more important than being clever.
3. The measure of good code is how long it takes someone else to understand it.
4. Simplicity trumps familiarity.

## Naming Rules
1. Use specific, precise names that convey meaning.
2. Choose names at appropriate level of abstraction.
3. Add prefixes/suffixes when they add vital information.
4. Establish consistent naming patterns within your codebase.
5. Use descriptive verbs for function names.
6. Avoid ambiguous abbreviations and acronyms.
7. Prioritize clarity over brevity.

**Example - Poor naming:**
```python
def get_data(d, t, fl):
    r = []
    for i in d:
        if i['tp'] == t and i['age'] > 30 and i['act'] == fl:
            r.append(i)
    return r
```

**Example - Good naming:**
```python
def filter_active_users_by_type(users, user_type, is_active):
    """Return users of specified type who are active and over 30."""
    filtered_users = []
    for user in users:
        if (user['type'] == user_type and 
            user['age'] > 30 and 
            user['active'] == is_active):
            filtered_users.append(user)
    return filtered_users
```

## Aesthetics
1. Use consistent formatting throughout the codebase.
2. Break code into logical "paragraphs" separated by blank lines.
3. Align code with similar structure when it helps identify patterns.
4. Use whitespace strategically to group related items.
5. Keep lines short enough to be easily scanned.
6. Maintain consistent indentation.

**Example - Poor aesthetics:**
```python
def calculate_total(items,tax,discount):
    result=0;
    for i in range(len(items)):result+=items[i]
    if discount:result*=0.9
    return result*(1+tax)
```

**Example - Good aesthetics:**
```python
def calculate_total(items, tax, apply_discount):
    """Calculate the total price with tax and optional discount."""
    subtotal = 0
    
    # Sum all item prices
    for item in items:
        subtotal += item
    
    # Apply discount if needed
    if apply_discount:
        subtotal *= 0.9  # 10% discount
    
    # Add tax and return
    return subtotal * (1 + tax)
```

## Comments Rules
1. Focus on explaining "why" not "what" in comments.
2. Document assumptions, edge cases, and non-obvious constraints.
3. Use "director commentary" to explain tricky sections.
4. Add summary comments for complex blocks of code.
5. Avoid redundant comments that repeat the code.
6. Use markers like TODO, FIXME consistently.
7. Update comments when code changes.

**Example - Poor comments:**
```python
# This function adds a and b
def add(a, b):
    return a + b  # Return the sum

# Loop through array
for i in range(len(data)):
    # Get the current item
    item = data[i]
    # Process item
    process(item)
```

**Example - Good comments:**
```python
# Apply Gaussian blur using a 9-step approximation
# instead of the full algorithm for performance reasons
def fast_gaussian_blur(image_data, radius):
    # Early return for edge case - no blur needed
    if radius < 1:
        return image_data
    
    # Implementation works in-place to avoid extra allocations
    # which is critical for large images
    # ... implementation ...
    
    return image_data
```

## Simplification Rules
1. Break down complex expressions into simpler components.
2. Use named variables to document intermediate values.
3. Simplify boolean expressions when possible.
4. Avoid double negatives in conditions.
5. Use early returns to reduce nesting.
6. Replace complex loops with helper functions.
7. Eliminate unnecessary variables and code.

**Example - Poor simplification:**
```python
if not(not(user.is_active) or not(user.has_permission("admin"))):
    # Complex logic with double negatives
    do_something_for_active_admins()

def process_data(data):
    temp1 = 0
    for i in range(len(data)):
        temp2 = data[i] * 2
        if temp2 > 10:
            temp1 += temp2
    return temp1
```

**Example - Good simplification:**
```python
if user.is_active and user.has_permission("admin"):
    do_something_for_active_admins()

def process_data(data):
    sum_of_valid_doubles = 0
    for value in data:
        doubled = value * 2
        if doubled > 10:
            sum_of_valid_doubles += doubled
    return sum_of_valid_doubles
```

## Function Rules
1. Functions should do one thing well.
2. Extract unrelated subproblems into separate functions.
3. Keep functions small and focused.
4. Functions should operate at a single level of abstraction.
5. Choose argument order thoughtfully and consistently.
6. Minimize the number of function parameters.
7. Consider creating wrapper functions for common use cases.

**Example - Poor function design:**
```python
def process_user_data(user_id, update_profile=False, send_email=False, reset_password=False):
    user = get_user(user_id)
    
    if update_profile:
        # Update user profile logic
        user.name = request.form.get('name')
        user.email = request.form.get('email')
        user.save()
    
    if send_email:
        # Send email logic
        template = get_email_template('welcome')
        send_mail(user.email, template, {})
    
    if reset_password:
        # Reset password logic
        new_password = generate_password()
        user.set_password(new_password)
        user.save()
        send_mail(user.email, 'password_reset', {'password': new_password})
    
    return user
```

**Example - Good function design:**
```python
def get_user(user_id):
    return User.find(user_id)

def update_user_profile(user, profile_data):
    user.name = profile_data.get('name', user.name)
    user.email = profile_data.get('email', user.email)
    user.save()
    return user

def send_welcome_email(user):
    template = get_email_template('welcome')
    send_mail(user.email, template, {})

def reset_user_password(user):
    new_password = generate_password()
    user.set_password(new_password)
    user.save()
    send_mail(user.email, 'password_reset', {'password': new_password})
    return new_password
```

## Control Flow Rules
1. Minimize nesting depth in code.
2. Prefer positive conditionals over negative ones.
3. Handle the most common code path first.
4. Return early from functions when possible.
5. Avoid "clever" shortcuts that reduce readability.
6. Structure conditionals to minimize cognitive load.

**Example - Poor control flow:**
```python
def process_order(order):
    if order is not None:
        if order.is_valid():
            if len(order.get_items()) > 0:
                if order.get_customer().has_valid_payment():
                    # Process the order
                    execute_order(order)
                    send_confirmation(order)
                    update_inventory(order)
                else:
                    raise PaymentException("Invalid payment")
            else:
                raise OrderException("Empty order")
        else:
            raise ValidationException("Invalid order")
    else:
        raise ValueError("Order is None")
```

**Example - Good control flow:**
```python
def process_order(order):
    if order is None:
        raise ValueError("Order is None")
    
    if not order.is_valid():
        raise ValidationException("Invalid order")
    
    if not order.get_items():
        raise OrderException("Empty order")
    
    if not order.get_customer().has_valid_payment():
        raise PaymentException("Invalid payment")
    
    # Process the valid order
    execute_order(order)
    send_confirmation(order)
    update_inventory(order)
```

## Data Structure Rules
1. Use the simplest data structure that does the job.
2. Reduce variable scope as much as possible.
3. Prefer constants and immutable data when possible.
4. Define variables close to where they're used.
5. Make interfaces to your code "narrow and deep".
6. Design data structures that prevent errors.
7. Document expectations about data with assertions.

**Example - Poor data structure usage:**
```python
def process_user_data(user_data):
    # Using general-purpose dict with inconsistent structure
    result = {}
    
    if user_data["type"] == "employee":
        result["name"] = user_data["name"]
        result["salary"] = user_data.get("salary", 0)
        result["department"] = user_data["dept"]
        # Might forget to handle other properties
    elif user_data["type"] == "customer":
        result["name"] = user_data["name"]
        result["total_purchases"] = user_data.get("purchases", 0)
        result["last_purchase_date"] = user_data["last_purchase"]
        # Different structure for different types
    
    return result
```

**Example - Good data structure usage:**
```python
class Employee:
    def __init__(self, data):
        self.name = data["name"]
        self.salary = data.get("salary", 0)
        self.department = data["dept"]
        self.start_date = data.get("start_date")
    
    def get_annual_cost(self):
        """Calculate total annual cost including benefits."""
        return self.salary * 1.25  # Including benefits

class Customer:
    def __init__(self, data):
        self.name = data["name"]
        self.total_purchases = data.get("purchases", 0)
        self.last_purchase_date = data["last_purchase"]
        self.loyalty_points = data.get("loyalty_points", 0)
    
    def get_value_score(self):
        """Calculate customer value based on purchase history."""
        return self.total_purchases * 0.1 + self.loyalty_points * 0.5

def create_user_from_data(user_data):
    if user_data["type"] == "employee":
        return Employee(user_data)
    elif user_data["type"] == "customer":
        return Customer(user_data)
    raise ValueError(f"Unknown user type: {user_data['type']}")
```

## Organization Rules
1. Organize code from high level to low level.
2. Keep related actions together, unrelated actions separate.
3. Use consistent patterns for similar functionality.
4. Minimize state changes and side effects.
5. Place helper functions after the code that uses them.
6. Group related functions together.

**Example - Poor organization:**
```python
def process_data():
    # Mix of high-level and low-level operations
    data = []
    file = open('data.csv', 'r')
    lines = file.readlines()
    file.close()
    
    for line in lines:
        if line.strip():
            parts = line.strip().split(',')
            if len(parts) >= 3:
                name = parts[0]
                age = int(parts[1])
                salary = float(parts[2])
                
                # Complex business logic mixed with parsing
                if age > 30 and salary < 50000:
                    tax_rate = 0.15
                else:
                    tax_rate = 0.2
                
                net_salary = salary * (1 - tax_rate)
                data.append({'name': name, 'age': age, 'net_salary': net_salary})
    
    return data
```

**Example - Good organization:**
```python
def read_csv_file(file_path):
    """Read data from CSV file and return as list of lines."""
    with open(file_path, 'r') as file:
        return [line.strip() for line in file.readlines() if line.strip()]

def parse_employee_data(line):
    """Parse a CSV line into employee data dictionary."""
    parts = line.split(',')
    if len(parts) < 3:
        return None
    
    return {
        'name': parts[0],
        'age': int(parts[1]),
        'salary': float(parts[2])
    }

def calculate_tax_rate(employee):
    """Determine tax rate based on employee criteria."""
    if employee['age'] > 30 and employee['salary'] < 50000:
        return 0.15
    return 0.2

def calculate_net_salary(employee):
    """Calculate employee's net salary after taxes."""
    tax_rate = calculate_tax_rate(employee)
    return employee['salary'] * (1 - tax_rate)

def process_data():
    """Main function to process employee data."""
    lines = read_csv_file('data.csv')
    employees = []
    
    for line in lines:
        employee = parse_employee_data(line)
        if employee:
            employee['net_salary'] = calculate_net_salary(employee)
            employees.append(employee)
    
    return employees
```

## Error Handling Rules
1. Handle error cases explicitly.
2. Make error messages specific and actionable.
3. Fail early at the source of the problem.
4. Use defensive programming for unexpected inputs.
5. Add redundant checks for critical code.

**Example - Poor error handling:**
```python
def save_user_data(user):
    try:
        # Vague error handling
        database.save(user)
    except Exception as e:
        print("Error")
```

**Example - Good error handling:**
```python
def save_user_data(user):
    if user is None:
        raise ValueError("Cannot save None user")
    
    try:
        database.save(user)
    except DatabaseConnectionError as e:
        logger.error(f"Database connection failed while saving user {user.id}: {e}")
        raise ServiceError("Unable to save user due to database connection issue") from e
    except ValidationError as e:
        logger.warning(f"User validation failed for user {user.id}: {e}")
        raise InvalidUserDataError(f"User data is invalid: {e}") from e
    except Exception as e:
        logger.error(f"Unexpected error while saving user {user.id}", exc_info=True)
        raise ServiceError("User save operation failed") from e
```

## Testing Rules
1. Write tests for both expected and edge cases.
2. Make test failures easy to diagnose.
3. Test with realistic data.
4. Test the interface, not the implementation.
5. Write self-testing code where practical.

**Example - Poor test:**
```python
def test_user_registration():
    user = register_user("john", "password123", "john@example.com")
    assert user is not None
```

**Example - Good test:**
```python
def test_user_registration_with_valid_data():
    # Arrange
    username = "john_doe"
    password = "password123"
    email = "john@example.com"
    
    # Act
    user = register_user(username, password, email)
    
    # Assert
    assert user.username == username
    assert user.email == email
    assert user.is_active is True
    assert user.id is not None

def test_user_registration_with_duplicate_username():
    # Arrange - create a user first
    existing_username = "existing_user"
    User.objects.create(username=existing_username, email="existing@example.com")
    
    # Act & Assert
    with pytest.raises(DuplicateUsernameError) as excinfo:
        register_user(existing_username, "password123", "new@example.com")
    
    assert "Username already exists" in str(excinfo.value)
```

## Optimization Rules
1. Optimize for readability first, performance second.
2. Profile before optimizing.
3. Comment on non-obvious optimization techniques.
4. Keep optimized (less readable) code isolated.
5. Document performance goals and constraints.

**Example - Poor optimization:**
```python
# Unnecessarily complex optimization without explanation
def find_closest_points(points, target, k=5):
    return sorted(points, key=lambda p: (p[0]-target[0])**2+(p[1]-target[1])**2)[:k]
```

**Example - Good optimization:**
```python
def find_closest_points(points, target, k=5):
    """Find k points closest to target point using Euclidean distance.
    
    Implementation notes:
    - We avoid using sqrt() in the distance calculation as it's unnecessary
      for comparison and improves performance by ~40%
    - For large point sets (>10000), consider using a KD-tree instead
      which would change time complexity from O(n log n) to O(k log n)
    """
    def squared_distance(p):
        # Avoid unnecessary sqrt calculation for performance
        return (p[0] - target[0])**2 + (p[1] - target[1])**2
    
    # Sort by squared distance and return k closest points
    return sorted(points, key=squared_distance)[:k]
```

<!-- EOF -->