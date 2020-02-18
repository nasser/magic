MAGIC Progress
==============

The goal of MAGIC is to compile all of Clojure into MSIL bytecode. To do this, it must provide symbolizers and tests for every Clojure AST node. This list will be kept in sync with progress on the library.

<table>
<tr><th>Node</th><th>Example</th><th>Impl.</th><th>Tests</th></tr>
<tr>
  <td><code>:static-method</code></td>
  <td><code>(Foo/Bar a b c)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:instance-method</code></td>
  <td><code>(.Bar foo a b c)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:static-field</code></td>
  <td><code>Foo/Baz</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:instance-field</code></td>
  <td><code>(.baz foo)</code> &nbsp;
      <code>(.-baz foo)</code>
  </td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:static-property</code></td>
  <td><code>Foo/Qux</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:instance-property</code></td>
  <td><code>(.qux foo)</code> &nbsp;
      <code>(.-qux foo)</code>
  </td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:dynamic-method</code></td>
  <td><code>(.quz x a b c)</code>
  </td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:dynamic-zero-arity</code></td>
  <td><code>(.quz x)</code>
  </td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:dynamic-field</code></td>
  <td><code>(.-quz x)</code>
  </td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:with-meta</code></td>
  <td><code>^{:meta "data"} [1 2 3]</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:const</code></td>
  <td><code>7</code> &nbsp;
      <code>"7"</code>
  </td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:vector</code></td>
  <td><code>[1 2 3]</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:map</code></td>
  <td><code>{:foo "bar"}</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:set</code></td>
  <td><code>#{1 2 3 4}</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:local</code></td>
  <td><code>(let [a 5] <strong>a</strong>)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:var</code></td>
  <td><code>(<strong>str</strong> 5)</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:do</code></td>
  <td><code>(do a b c)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:if</code></td>
  <td><code>(if true then else)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:new</code></td>
  <td><code>(Type.)</code> &nbsp;
      <code>(Type. 1 2)</code> &nbsp;
      <code>(ValueType. 1 2)</code>
  </td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:initobj</code></td>
  <td><code>(ValueType.)</code> &nbsp;
      <code>(new ValueType)</code>
  </td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:quote</code></td>
  <td><code>(quote (+ 6 7))</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:set!</code></td>
  <td><code>(set! (.foo bar) 5)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:try</code></td>
  <td><code>(try (foo) (catch System.Exception e e))</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:catch</code></td>
  <td><code>(try (foo) <strong>(catch System.Exception e e)</strong>)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:throw</code></td>
  <td><code>(throw (System.Exception. "foo"))</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:letfn</code></td>
  <td><code>(letfn [(foo [a] a)] (foo 2))</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:let</code></td>
  <td><code>(let [a 5] a)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:loop</code></td>
  <td><code>(loop [a 1] (recur (inc a)))</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:recur</code></td>
  <td><code>(loop [a 1] <strong>(recur (inc a)</strong>))</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:binding</code></td>
  <td><code>(let [a <strong>5</strong>] a)</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:fn</code></td>
  <td><code>(fn [x] x)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:fn-method</code></td>
  <td><code>(fn ([x] x))</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:def</code></td>
  <td><code>(def a 1)</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:invoke</code></td>
  <td><code>(str 1 2)</code></td>
  <td> ✔ </td>
  <td> ✔ </td>
</tr>
<tr>
  <td><code>:the-var</code></td>
  <td><code>(var clojure.core/str)</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:reify</code></td>
  <td><code>(reify IInterface (foo [a b] ...)) </code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:deftype</code></td>
  <td><code>(deftype Foo [a b] IInterface (Bar [this x] ...))</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:proxy</code></td>
  <td><code>(proxy [Type IInterface] [a b] (Foo [ this x] a ...))</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:case</code></td>
  <td><code>(case foo 1 :one 2 :two)</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:import</code></td>
  <td><code>(import Type [Namespace Type])</code></td>
  <td>   </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:monitor-enter</code></td>
  <td><code>(monitor-enter lock)</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
<tr>
  <td><code>:monitor-exit</code></td>
  <td><code>(monitor-exit lock)</code></td>
  <td> ✔ </td>
  <td>   </td>
</tr>
</table>
