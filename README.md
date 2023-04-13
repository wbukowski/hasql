
HaSQL - Haskell powered DBMS
=====
Wiktor Bukowski
---------------

Final project for Functional Programming in winter term of year 2022/2023.

## Contents

- [Conventions](#conventions)
- [Usage](#usage)
    - [SELECT](#select)
    - [CREATE](#create)
    - [DROP](#drop)
    - [INSERT](#insert)
    - [UPDATE](#update)
    - [DELETE](#delete)
- [Known bugs](#bugs)


## <a name="conventions"> Conventions

- Table and column names must start with a letter and include only letters, digits and underscores. Capitalization does not matter.

- Table names can be followed by alias satisfying the same requirements as in the point above. Aliases are created with a keyword "AS" followed by wanted alias.

- Field names in predicates may (but don't have to) be written with table name / alias. Field may not have an alias only if there's no more tables containing the same field name in the query. Aliasing the field is achieved by writing the alias and field name separated by a single dot.

- Database supports integers and fixed size strings (size is defined uppon creation of the table, can be different for different columns).

- Formally, tuple cannot contain NULL value. However, one may insert a tuple to database without specifying some of its fields. Field contents for int and string would be 0 or '' respectfully.

- Query predicates are built from expressions, unary operator "NOT" and binary operators "AND" and "OR". Expressions consist of binary comparators (such as =, <, >, <=, =>, !=, <> or "LIKE"), operations (addition, subtraction, multiplication, dividing and modulo operation), numeric and string literals or variables (being column names).


## <a name="usage"> Usage


### <a name="select"> SELECT

```
SELECT *columnspecifier* FROM [list of table names] (WHERE *predicate*)
```

### <a name="create"> CREATE

```
CREATE TABLE *tablename* (column1 datatype1, column2 datatype2, ...)
```

### <a name="drop"> DROP

```
DROP TABLE *tablename*
```

### <a name="insert"> INSERT

```
INSERT INTO *tablename* (column1, column2, ...) VALUES (value1, value2, ...)
```

(when inserting values for all fields)
```
INSERT INTO *tablename* VALUES (value1, value2, ...)
```

### <a name="update"> UPDATE

```
UPDATE *tablename* SET column1 = value1, column2 = value2, ... WHERE *predicate*
```

### <a name="delete"> DELETE

```
DELETE FROM *tablename* WHERE *predicate*
```

## <a name="bugs"> Known bugs

- Comparing two variables of string fields produces type errors (type classes instances)

- Predicates in joins happen to work (streams)
