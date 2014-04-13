GObject Introspection

# 1. Main interface

This is Gobject FFI.

Usage example:

```racket
(defvar *gtk* (gir:ffi "Gtk"))                  
(gir:call *gtk* 'init 0 (cffi:null-pointer))    
(let ((window (gir:call *gtk* "Window" 'new 0)))
  (gir:call window 'show)                       
  (gir:call *gtk* 'main))                       
```

Interface with the GObjectIntrospection is based on repositories. Main
function is

```racket
(ffi repository-name [version]) -> repository
  repository-name : string                 
  version : string = nil                   
```

Returns interface to repository with name `repository-name`. In current implementation repository is function.

# 2. Using interface objects

Every interface object, returned by `ffi`, or other objects can be
used in function `call` as you can see in the example above. In this
version `call` is simply alias to `cl:funcall`, but in future versions
this may change.

# 3. Get FFI element

```racket
(call repository func-name func-arg ...) -> any          
  repositry : repository
  func-name : (or string symbol)                    
  func-arg : any                                    
(call repository const-name) -> any                      
  repositry : repository
  const-name : (or string symbol)                   
(call repository enum-name enum-value-name) -> integer   
  repositry : repository
  enum-name : (or string symbol)                    
  enum-value-name : (or string symbol)              
(call repository class-name constructor-name) -> function
  repositry : repository
  class-name : (or string symbol)                   
  constructor-name : (or string symbol)             
```

This interface takes as a first argument name of foreign object. Name
could be `string` or `symbol`. In both cases it’s allowed to replace
"\_" with "-". So you can write either "get\_name" or ’get-name with the
same result. If you use symbol its name is downcased.

If first argument is a name of function, then rest arguments are the
arguments of the function and it returns result of the function. In
example

```racket
(defvar *gtk* (ffi "Gtk"))              
(call *gtk* 'init 0 (cffi:null-pointer))
```

gtk\_init is called with 0 and null pointer.

If first argument is a name of constant, then it returns value of the
constant. For example,

```racket
(call *gtk* "MAJOR-VERSION")
```

returns 2 for GTK2 or 3 for GTK3.

If first argument is a name of enumeration, then second arguments should
be value name. It returns integer value. Value name must be `keyword`.
Any other symbol or string will mean, that you want call a method with
that name.

For example,

```racket
(call *gtk* "WindowType" :toplevel)
```

Returns 0.

If first argument is a name of class (or struct), then the second
argument should be a name of class constructor (in GTK it is usually
"new"), rest arguments are the arguments of the constructor. In GTK
classes have names, beginning with capital char, so you have to use
either string or symbol like ’|Window|.

```racket
(defvar *window* (gir:call *gtk* "Window" 'new 0))
```

This call will return a representation of object.

# 4. Foreign objects

```racket
(call object method-name method-arg ...) -> any
  object : gir-object                          
  method-name : (or string symbol)             
  method-arg : any                             
```

Representation of an object is also a function. First argument of it
should be either name of method (`string` or `symbol`) or keyword with
special meaning.

```racket
(call *window* 'add button)
```

will call method "add" with argument in variable "button".

## 4.1. Pointer to object

To get C pointer to an object call it with "method" :this.

```racket
(call *window* :this)
```

It is possible to make an object from a pointer:

```racket
(defvar *window-from-ptr* (call *gtk* "Window" window-ptr))
```

`window-ptr` should be `cffi:foreign-pointer` here.

## 4.2. Fields

Getting and setting field values are done with :field and :set-field!.

```racket
(defvar *entry* (call *gtk* "TargetEntry" 'new "ok" 0 0))
                                                         
> (call *entry* :field 'flags)                           
0                                                        
> (call *entry* :set-field! 'flags 1)                    
> (call *entry* :field 'flags)                           
1                                                        
```

But you cannot set with :set-field! complex types such as structs,
unions or even strings. It is a restriction of GObjectIntrospection.

## 4.3. Properties

Getting and setting field values are done with :properties and
:set-properties!. You may get or set several properties at once.

```racket
(multiple-value-bind (width height)                                    
  (call *window* :properties 'width-request 'height-request)           
  ...)                                                                 
(call *window* :set-properties! 'width-request 100 'height-request 200)
```

# 5. Signals

```racket
(connect object signal-name handler) -> void?               
  object : gir-object                                       
  signal-name : (or symbol string)                          
  handler : (or function cffi:foreign-pointer string symbol)
```

Connects signal handler to object. If handler is a string o symbol, then
it denotes C-function.
