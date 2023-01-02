# cl-gobject-introspection

Common Lisp bindings to [gobject-introspection](https://gitlab.gnome.org/GNOME/gobject-introspection).

## Motivational examples

### GTK+ 3

```lisp
(defvar *gtk* (gir:require-namespace "Gtk" "3.0"))
(gir:invoke (*gtk* 'init) nil)
(let ((window (gir:invoke (*gtk* "Window" 'new)
                          (gir:nget *gtk* "WindowType" :toplevel))))
  (gir:invoke (window 'show))
  (gir:invoke (*gtk* 'main)))
```

### Gtk 4

```lisp
(defvar *gio* (gir:require-namespace "Gio"))
(defvar *gtk* (gir:require-namespace "Gtk" "4.0"))

(let ((app (gir:invoke (*gtk* "Application" 'new)
		       "org.gtk.example"
		       (gir:nget *gio* "ApplicationFlags" :default-flags))))
  (gir:connect app "activate"
	       (lambda (app)
		 (let ((window (gir:invoke (*gtk* "ApplicationWindow" 'new) app)))
		   (gir:invoke (window 'show)))))
  (gir:invoke (app 'run) nil))
```

More examples may be found in an `examples` directory.

### Hello, World!

![Hello, World screenshot](https://raw.githubusercontent.com/andy128k/cl-gobject-introspection/master/examples/screenshots/screenshot-hello-world.png)

### Flood Game

![Flood Game screenshot](https://raw.githubusercontent.com/andy128k/cl-gobject-introspection/master/examples/screenshots/screenshot-flood-game.png)

### Maze

![Maze screenshot](https://raw.githubusercontent.com/andy128k/cl-gobject-introspection/master/examples/screenshots/screenshot-maze.png)

## 1. Main interface

Interface with the GObjectIntrospection is based on repositories. Main
function which creates an instance of a repository is

```lisp
(gir:require-namespace repository-name [version])
```

Examples:

```lisp
(gir:require-namespace "GLib")

(gir:require-namespace "Gtk" "4.0")
```

Returns interface to repository with name `repository-name`.

## 2. Using interface objects

In gobject object system, repository, class, enumeration are all kind
of namespace.  To get a function/const/enum/class from the repository
is kind of namespace access, to get a method from object is kind of
namespace access too.  So that concept is introduced into
cl-gobject-introspection too.  A function named nget (namespace get)
is use to do namespace accessing as follow,

```lisp
(gir:nget *gtk* "WindowType" :toplevel) ; get enum value
(gir:nget *gtk* "Window" 'new)          ; get a class constructor function
(gir:nget *window* 'add)                ; get a method
```

To call a method of object, we need to get the method from function,
then call it.

```lisp
(funcall (gir:nget *gtk* "Window" 'new) 0)
```

To save some typing, a macro named `invoke` is introduced as follow,

```lisp
(gir:invoke (*gtk* "Window" 'new) 0)
```

In this way, `(*gtk* "Window" 'new)` can be seen as function and `0` can
be seen as parameters.

## 3. Get FFI element

```lisp
(gir:invoke (repository func-name) func-arg ...) -> any
  repositry : repository
  func-name : (or string symbol)
  func-arg : any
(gir:nget repository const-name) -> any
  repository : repository
  const-name : (or string symbol)
(gir:nget repository enum-name enum-value-name) -> integer
  repositry : repository
  enum-name : (or string symbol)
  enum-value-name : keyword
(gir:nget repository class-name constructor-name) -> function
  repositry : repository
  class-name : (or string symbol)
  constructor-name : (or string symbol)
```

The `nget` takes all arguments except the first one as names of foreign
objects. Name could be a `string` or a `symbol`. In both cases it is
allowed to replace `_` with `-`. So you can write either `"get_name"`
or `’get-name` with the same result. If you use symbol its name is
downcased.

If second argument of nget is a name of function, nget will get the
function object.  And we can use invoke macro for more concise syntax.

```lisp
(defvar *gtk* (gir:require-namespace "Gtk" "3.0"))
(gir:invoke (*gtk* 'init) nil)
```

gtk\_init is called with an empty array.

If second argument of nget is a name of constant, then it returns
value of the constant. For example,

```lisp
(gir:nget *gtk* "MAJOR-VERSION")
```

returns 2 for GTK2 or 3 for GTK3.

If second argument of nget is a name of enumeration, then third
argument should be value name. It returns integer value. Value name
must be `keyword`.  Any other symbol or string will mean, that you
want get a method with that name.

For example,

```lisp
(gir:nget *gtk* "WindowType" :toplevel)
```

Returns 0.

If second argument of nget is a name of class (or struct), then the
third argument should be a name of class constructor (in GTK it is
usually "new"), and we can use invoke here too. In GTK classes have
names beginning with capital char, so you have to use either string
or symbol like ’|Window|.

```lisp
(defvar *window* (gir:invoke (*gtk* "Window" 'new) 0))
```

This call will return a representation of object.

## 4. Foreign objects

```lisp
(gir:invoke (object method-name) method-arg ...) -> any
  object : gir-object
  method-name : (or string symbol)
  method-arg : any
```

To get the method of an object, the second argument of nget should be
either name of method (`string` or `symbol`) or keyword with special
meaning.  Invoke can be used for the more concise syntax.

```lisp
(gir:invoke (*window* 'add) button)
```

will call method "add" with argument in variable "button".

### 4.1. Pointer to object

To get C pointer to an object, use `this-of`.

```lisp
(gir::this-of *window*)
```

It is possible to make an object from a pointer:

```lisp
(defvar *window-from-ptr* (gir:build-object-ptr (gir:nget *gtk* "Window") window-ptr))
```

`window-ptr` should be `cffi:foreign-pointer` here.

### 4.2. Fields

Getting and setting field values are done with field and setf.

```lisp
(defvar *entry* (gir:invoke (*gtk* "TargetEntry" 'new) "ok" 0 0))
                                                         
> (gir:field *entry* 'flags)
0
> (setf (gir:field *entry* 'flags) 1)
> (gir:field *entry* 'flags)
1
```

But you cannot set with :set-field! complex types such as structs,
unions or even strings. It is a restriction of GObjectIntrospection.

### 4.3. Properties

Getting and setting property are done with property and setf.

```lisp
(gir:property *window* 'width-request)
(setf (gir:property *window* 'width-request) 100)
```

## 5. Signals

```lisp
(gir:connect object signal-name handler) -> void?
  object : gir-object
  signal-name : (or symbol string)
  handler : (or function cffi:foreign-pointer string symbol)
```

Connects signal handler to object. If handler is a string o symbol, then
it denotes C-function.

## 6. Description

Various description information, such as a signature of a function, can be
gotten via the description functions.

```lisp
(gir:nget-desc *gtk* 'init)
#F<init(#V<argv: (SEQUENCE STRING)>): (#V<RETURN-VALUE: VOID>
                                       #V<argv: (SEQUENCE STRING)>)>
```
