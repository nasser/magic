MAGIC
=====
Functional compiler library for ClojureCLR

Status
------
Experimental, unfinished, way pre-alpha. Here be dragons.

Strategy
--------
MAGIC consumes AST nodes from the stock ClojureCLR compiler. It turns those AST nodes into MAGE bytecode to be compiled into executable MSIL. By using the existing ClojureCLR reader and analyzer, we avoid rewriting most of what is already a high performance, high quality compiler. By using MAGE bytecode, we are granted the full power of Clojure to reason about generating bytecode from Clojure forms.

### Symbolizers
Clojure forms are turned into MAGE bytecode using *symbolizers*. Symbolizers are stored as a map of AST node type to `fn`s taking an AST node and a symbolizer map, and returning symbolic MAGE bytecode (hence the name symbolizer).

```clojure
{ BooleanExpr (fn boolean-symbolizer [this symbolizers]
                (let [v (:_val (data-map this))]
                  (if v
                    (il/ldc-i4-1)
                    (il/ldc-i4-0)))) }
```

Rationale and History
---------------------
During the development of [Arcadia](https://github.com/arcadia-unity/Arcadia), it was found that binaries produced by the ClojureCLR compiler did not survive Unity's AOT compilation process to its more restrictive export targets, particularly iOS, WebGL, and the PlayStation. While it is understood that certain more 'dynamic' features of C# are generally not supported on these platforms, the exact cause of the failures is difficult to pinpoint. MAGIC was built primarily to support a well reasoned approach to Arcadia export.

Name
----
MAGIC stands for Morgan And Grand IL Compiler. It is named after the [Morgan Avenue and Grand Street intersection](https://www.google.com/maps/place/Grand+St+%26+Morgan+Ave,+Brooklyn,+NY+11237/@40.7133714,-73.9348001,17z/data=!3m1!4b1!4m2!3m1!1s0x89c25eab5ea3b021:0x77aaab63f0e3d135) in Brooklyn, the location of the [Kitchen Table Coders](http://kitchentablecoders.com/) studio where the library was developed.

Legal
-----
Copyright © 2015 Ramsey Nasser

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

```
http://www.apache.org/licenses/LICENSE-2.0
```

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.