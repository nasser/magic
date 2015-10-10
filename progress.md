MAGIC Progress
==============

The goal of MAGIC is to compile all of Clojure into MSIL bytecode. To do this, it must provide symbolizers and tests for every Clojure AST node. This list will be kept in sync with progress on the library.

<table>
<tr><th>Node</th><th>Example</th><th>Symbolizer</th></tr>
<tr>
  <td><code>:let</code></td>
  <td><code>(let [a] a)</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:fn</code></td>
  <td><code>(fn [a] a)</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:host-field</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:do</code></td>
  <td><code>(do a b c)</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:maybe-host-form</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:maybe-class</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:if</code></td>
  <td><code>(if test then else)</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:fn-method</code></td>
  <td><code> </code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:new</code></td>
  <td><code>(new Type a b)</code>,
      <code>(Type. a b)</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:recur</code></td>
  <td><code>(loop [a] (recur a))</code>,
      <code>(fn [a] (recur a))</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:with-meta</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:letfn</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:vector</code></td>
  <td><code>[1 2 3]</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:loop</code></td>
  <td><code>(loop [a] (recur a))</code>,
      <code>(fn [a] (recur a))</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:binding</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:const</code></td>
  <td><code>12</code>,
      <code>"hello"</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:set!</code></td>
  <td><code>(set! (.a b) c)</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:var</code></td>
  <td><code> </code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:quote</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:host-interop</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:catch</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:invoke</code></td>
  <td><code>(a b)</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:throw</code></td>
  <td><code>(throw (Exception. "foo"))</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:def</code></td>
  <td><code>(defn sym val)</code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:try</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:set</code></td>
  <td><code>#{1 2 3}</code></td>
  <td> ✔︎ </td>
</tr>
<tr>
  <td><code>:host-call</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:local</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:the-var</code></td>
  <td><code> </code></td>
  <td>   </td>
</tr>
<tr>
  <td><code>:map</code></td>
  <td><code>{:foo 'bar}</code></td>
  <td> ✔︎ </td>
</tr>
</table>