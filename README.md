GObject Introspection

# 1. Main interface

This is Gobject FFI.

Usage example:

```racket
(defvar *gtk* (gir:ffi "Gtk"))                  
(gir:invoke (*gtk* 'init) nil)
(let ((window (gir:invoke (*gtk* "Window" 'new)
                          (gir:nget *gtk* "WindowType" :toplevel))))
  (gir:invoke (window 'show))
  (gir:invoke (*gtk* 'main)))
```

Interface with the GObjectIntrospection is based on repositories. Main
function is

```racket
(gir:ffi repository-name [version]) -> repository
  repository-name : string                 
  version : string = nil                   
```

Returns interface to repository with name `repository-name`. In
current implementation repository is function.

# 2. Using interface objects

In gobject object system, repository, class, enumeration are all kind
of namespace.  To get a function/const/enum/class from the repository
is kind of namespace access, to get a method from object is kind of
namespace access too.  So that concept is introduced into
cl-gobject-introspection too.  A function named nget (namespace get)
is use to do namespace accessing as follow,

```racket
(gir:nget *gtk* "WindowType" :toplevel) -> get enum value
(gir:nget *gtk* "Window" 'new) -> get a class constructor function
(gir:nget *window* 'add) -> get a method
```

To call a method of object, we need to get the method from function,
then call it.

```racket
(funcall (gir:nget *gtk* "Window" 'new) 0)
```

To save some typing, a macro named invoke is introduced as follow,

```racket
(gir:invoke (*gtk* "Window" 'new) 0)
```

In this way, (\*gtk\* "Window" 'new) can be seen as function and "0" can
be seen as parameters.

# 3. Get FFI element

```racket
(gir:invoke (repository func-name) func-arg ...) -> any
  repositry : repository
  func-name : (or string symbol)                    
  func-arg : any                                    
(gir:nget repository const-name) -> any
  repositry : repository
  const-name : (or string symbol)                   
(gir:nget repository enum-name enum-value-name) -> integer
  repositry : repository
  enum-name : (or string symbol)                    
  enum-value-name : (or string symbol)              
(gir:nget repository class-name constructor-name) -> function
  repositry : repository
  class-name : (or string symbol)                   
  constructor-name : (or string symbol)             
```

The nget takes all arguments except the first one as names of foreign
objects. Name could be `string` or `symbol`. In both cases it’s
allowed to replace "\_" with "-". So you can write either "get\_name"
or ’get-name with the same result. If you use symbol its name is
downcased.

If second argument of nget is a name of function, nget will get the
function object.  And we can use invoke macro for more concise syntax.

```racket
(defvar *gtk* (gir:ffi "Gtk"))
(gir:invoke (*gtk* 'init) nil)
```

gtk\_init is called with an empty array.

If second argument of nget is a name of constant, then it returns
value of the constant. For example,

```racket
(gir:nget *gtk* "MAJOR-VERSION")
```

returns 2 for GTK2 or 3 for GTK3.

If second argument of nget is a name of enumeration, then third
argument should be value name. It returns integer value. Value name
must be `keyword`.  Any other symbol or string will mean, that you
want get a method with that name.

For example,

```racket
(gir:nget *gtk* "WindowType" :toplevel)
```

Returns 0.

If second argument of nget is a name of class (or struct), then the
third argument should be a name of class constructor (in GTK it is
usually "new"), and we can use invoke here too. In GTK classes have
names beginning with capital char, so you have to use either string
or symbol like ’|Window|.

```racket
(defvar *window* (gir:invoke (*gtk* "Window" 'new) 0))
```

This call will return a representation of object.

# 4. Foreign objects

```racket
(gir:invoke (object method-name) method-arg ...) -> any
  object : gir-object                          
  method-name : (or string symbol)             
  method-arg : any                             
```

To get the method of an object, the second argument of nget should be
either name of method (`string` or `symbol`) or keyword with special
meaning.  Invoke can be used for the more concise syntax.

```racket
(gir:invoke (*window* 'add) button)
```

will call method "add" with argument in variable "button".

## 4.1. Pointer to object

To get C pointer to an object, use object-this.

```racket
(gir::object-this *window*)
```

It is possible to make an object from a pointer:

```racket
(defvar *window-from-ptr* (funcall (gir:nget *gtk* "Window") window-ptr))
```

`window-ptr` should be `cffi:foreign-pointer` here.

## 4.2. Fields

Getting and setting field values are done with field and setf.

```racket
(defvar *entry* (gir:invoke (*gtk* "TargetEntry" 'new) "ok" 0 0))
                                                         
> (gir:field *entry* 'flags)
0                                                        
> (setf (gir:field *entry* 'flags) 1)
> (gir:field *entry* 'flags)
1                                                        
```

But you cannot set with :set-field! complex types such as structs,
unions or even strings. It is a restriction of GObjectIntrospection.

## 4.3. Properties

Getting and setting property are done with property and setf.

```racket
(gir:property *window* 'width-request)
(setf (gir:property *window* 'width-request) 100)
```

# 5. Signals

```racket
(gir:connect object signal-name handler) -> void?
  object : gir-object                                       
  signal-name : (or symbol string)                          
  handler : (or function cffi:foreign-pointer string symbol)
```

Connects signal handler to object. If handler is a string o symbol, then
it denotes C-function.
