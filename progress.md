MAGIC Progress
==============

The goal of MAGIC is to compile all of Clojure into MSIL bytecode. To do this, it must provide symbolizers and tests for every Clojure AST node. This list will be kept in sync with progress on the library.

<table>
<tr><th>Node</th><th>Example</th><th>Symbolizer</th></tr>
<tr>
  <td>AssignExpr</td>
  <td><code>(set! a b)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>BodyExpr</td>
  <td><code>(do a b c)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>BooleanExpr</td>
  <td><code>false</code>, <code>true</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>CaseExpr</td>
  <td><code>(case 1 1 :a 2 :b)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>ConstantExpr</td>
  <td><code>(quote a)</code>, <code>Type</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>DefExpr</td>
  <td><code>(def a 1)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>EmptyExpr</td>
  <td><code>[]</code>, <code>()</code>, <code>{}</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>FnExpr</td>
  <td><code>(fn [a] a)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>FnMethodExpr</td>
  <td><code>([a b] (+ a b))</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>IfExpr</td>
  <td><code>(if a b c)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>ImportExpr</td>
  <td><code>(import Type)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>InstanceFieldExpr</td>
  <td><code>(.field obj)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>InstancePropertyExpr</td>
  <td><code>(.prop obj)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>InstanceMethodExpr</td>
  <td><code>(.method obj a b)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>InstanceOfExpr</td>
  <td><code>(instance? Type foo)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>InstanceZeroArityCallExpr</td>
  <td><code>(.method obj)</code></td>
  <td><center>   </center></td>
</tr>
<tr>
  <td>InvokeExpr</td>
  <td><code>(a b)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>KeywordExpr</td>
  <td><code>:foo</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>KeywordInvokeExpr</td>
  <td><code>(:foo a)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>LetExpr</td>
  <td><code>(let [a 1] a)</code>,<br><code>(loop [a 1] (recur a))</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>LetFnExpr</td>
  <td><code>(letfn [a (fn [] (b))<br>b (fn [])])</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>LiteralExpr</td>
  <td><code>54</code>, <code>"foo"</code>, <code>nil</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>LocalBindingExpr</td>
  <td><code>(let [a 1] a)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>MapExpr</td>
  <td><code>{:a 1 :b 2}</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>MetaExpr</td>
  <td>    </td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>MethodExpr</td>
  <td><code>(Foo/Bar a)</code>, <code>(.foo a)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>MonitorEnterExpr</td>
  <td><code>(monitor-enter x)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>MonitorExitExpr</td>
  <td><code>(monitor-exit x)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>NewClassInstanceExpr</td>
  <td><code>(defclass Foo ...)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>NewExpr</td>
  <td><code>(new Type)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>NewInstanceExpr</td>
  <td><code>(deftype Foo ...)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>NilExpr</td>
  <td><code>nil</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>NumberExpr</td>
  <td><code>15</code>, <code>4.5</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>RecurExpr</td>
  <td><code>(recur 1 2)</code></td>
  <td><center>  </center></td>
</tr>
<tr>
  <td>SetExpr</td>
  <td><code>#{:a :b :c}</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>StaticFieldExpr</td>
  <td><code>Math/PI</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>StaticMethodExpr</td>
  <td><code>(Foo/Bar)</code>, <code>(Foo/Bar a)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>StaticPropertyExpr</td>
  <td><code>Time/time</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>StringExpr</td>
  <td><code>"foo"</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>TheVarExpr</td>
  <td><code>(var +)</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>ThrowExpr</td>
  <td><code>(throw (Exception. "foo"))</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>TryExpr</td>
  <td><code>(try<br>&nbsp;&nbsp;(foo) (catch Exception e))</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>UnresolvedVarExpr</td>
  <td>    </td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>VarExpr</td>
  <td><code>+</code>, <code>magic.core/analyze</code></td>
  <td><center> ✔︎ </center></td>
</tr>
<tr>
  <td>VectorExpr</td>
  <td><code>[1 2 3]</code></td>
  <td><center> ✔︎ </center></td>
</tr>
</table>