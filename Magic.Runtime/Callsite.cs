using System;
using System.Reflection;
using System.Linq;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Magic
{
    // backported for .NET 3.5 support
    public delegate TResult CallsiteFunc<in T1,out TResult>(T1 arg1);
    public delegate TResult CallsiteFunc<in T1,in T2,out TResult>(T1 arg1, T2 arg2);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,out TResult>(T1 arg1, T2 arg2, T3 arg3);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19);
    public delegate TResult CallsiteFunc<in T1,in T2,in T3,in T4,in T5,in T6,in T7,in T8,in T9,in T10,in T11,in T12,in T13,in T14,in T15,in T16,in T17,in T18,in T19,in T20,out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7, T8 arg8, T9 arg9, T10 arg10, T11 arg11, T12 arg12, T13 arg13, T14 arg14, T15 arg15, T16 arg16, T17 arg17, T18 arg18, T19 arg19, T20 arg20);
    public static partial class DelegateHelpers
    {
        // high performance well typed delegate
        // fast but require combinatorial explosion of compiler generated
        // dummy code to get around generic limitations on il2cpp
        // here be dragons...

        // public static T CreateDelegate<T>(MethodInfo method) where T : Delegate
        // {
        //     return (T)Delegate.CreateDelegate(typeof(T), null, method);
        // }
        // public static Func<object, object> GetDelegateWeakFromStrong01<A, Z>(MethodInfo method)
        // {
        //     var func = CreateDelegate<Func<A, Z>>(method);
        //     Func<object, object> ret = a => func((A)a);
        //     return ret;
        // }

        // public static Func<object, object> GetMethodDelegate01(MethodInfo method)
        // {
        //     Func<object, object> ret;
        //     if(method.IsStatic)
        //         ret = arg1 => method.Invoke(null, new[] { arg1 });
        //     else
        //         ret = target => method.Invoke(target, null);
        //     return ret;
        // }

        // public static Func<object, object, object> GetMethodDelegate02(MethodInfo method)
        // {
        //     Func<object, object, object> ret;
        //     if(method.IsStatic)
        //         ret = (arg1, arg2) => method.Invoke(null, new[] { arg1, arg2 });
        //     else
        //         ret = (target, arg1) => method.Invoke(target, new [] { arg1 });
        //     return ret;
        // }

        public static Func<object, object> GetZeroArityDelegate(FieldInfo field)
        {
            return obj => field.GetValue(obj);
        }

        public static Func<object, object> GetZeroArityDelegate(MethodInfo method)
        {
            return obj => method.Invoke(obj, null);
        }

        public static Func<object, object> GetZeroArityDelegate(PropertyInfo property)
        {
            return GetZeroArityDelegate(property.GetGetMethod());
        }

        public static Func<object, object, object> GetSetMemberDelegate(FieldInfo field)
        {
            if(field.FieldType.IsPrimitive)
                return (obj, val) => {
                    val = Convert.ChangeType(val, field.FieldType);
                    field.SetValue(obj, val);
                    return val;
                };
            else
                return (obj, val) => {
                    field.SetValue(obj, val);
                    return val;
                };
        }

        public static Func<object, object, object> GetSetMemberDelegate(PropertyInfo property)
        {
            if(property.PropertyType.IsPrimitive)
                return (obj, val) => {
                    val = Convert.ChangeType(val, property.PropertyType);
                    property.SetValue(obj, val, null);
                    return val;
                };
            else
                return (obj, val) => {
                    property.SetValue(obj, val, null);
                    return val;
                };
        }

    //     public static Func<object, object> GetDelegateWeak01(MethodInfo method)
    //     {
    //         MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong01), BindingFlags.Public | BindingFlags.Static);
    //         MethodInfo constructedHelper;
    //         var parameters = method.GetParameters();
    //         if(method.IsStatic)
    //         {
    //             Debug.Assert(parameters.Length == 1);
    //             constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, method.ReturnType);
    //         }
    //         else
    //         {
    //             Debug.Assert(parameters.Length == 0);
    //             constructedHelper = helper.MakeGenericMethod(method.DeclaringType, method.ReturnType);
    //         }
    //         object ret = constructedHelper.Invoke(null, new object[] { method });
    //         return (Func<object, object>)ret;
    //     }

    //     public static Func<object, object, object> GetDelegateWeakFromStrong02<A, B, Z>(MethodInfo method)
    //     {
    //         Console.WriteLine("[GetDelegateWeakFromStrong02] {0} {1} {2} {3}", method, typeof(A), typeof(B), typeof(Z));
    //         var func = CreateDelegate<Func<A, B, Z>>(method);
    //         Func<object, object, object> ret = (a, b) => func((A)a, (B)b);
    //         return ret;
    //     }

    //     public static Func<object, object, object> GetDelegateWeak02(MethodInfo method)
    //     {
    //         MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong02), BindingFlags.Public | BindingFlags.Static);
    //         MethodInfo constructedHelper;
    //         var parameters = method.GetParameters();
    //         if(method.IsStatic)
    //         {
    //             Debug.Assert(parameters.Length == 2);
    //             constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, parameters[1].ParameterType, method.ReturnType);
    //         }
    //         else
    //         {
    //             Debug.Assert(parameters.Length == 1);
    //             constructedHelper = helper.MakeGenericMethod(method.DeclaringType, parameters[0].ParameterType, method.ReturnType);
    //         }
    //         object ret = constructedHelper.Invoke(null, new object[] { method });
    //         return (Func<object, object, object>)ret;
    //     }

    //     public static Func<object, object, object, object> GetDelegateWeakFromStrong03<A, B, C, Z>(MethodInfo method)
    //     {
    //         var func = CreateDelegate<Func<A, B, C, Z>>(method);
    //         Func<object, object, object, object> ret = (a, b, c) => func((A)a, (B)b, (C)c);
    //         return ret;
    //     }

    //     public static Func<object, object, object, object> GetDelegateWeak03(MethodInfo method)
    //     {
    //         MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong03), BindingFlags.Public | BindingFlags.Static);
    //         MethodInfo constructedHelper;
    //         var parameters = method.GetParameters();
    //         if(method.IsStatic)
    //         {
    //             Debug.Assert(parameters.Length == 3);
    //             constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, parameters[1].ParameterType, parameters[2].ParameterType, method.ReturnType);
    //         }
    //         else
    //         {
    //             Debug.Assert(parameters.Length == 2);
    //             constructedHelper = helper.MakeGenericMethod(method.DeclaringType, parameters[0].ParameterType, parameters[1].ParameterType, method.ReturnType);
    //         }
    //         object ret = constructedHelper.Invoke(null, new object[] { method });
    //         return (Func<object, object, object, object>)ret;
    //     }
    }

    // (set! (.name o) value)
    public class CallSiteSetMember
    {
        string MemberName;
        // TODO not actually overloaded on value type
        // might need custom cache type
        CallSiteCache02 cache;

        public CallSiteSetMember(string memberName)
        {
            MemberName = memberName;
            cache = new CallSiteCache02();
        }

        public object Invoke(object o, object value)
        {
            if(cache.TryGet(o, value, out var result))
                return result(o, value);
            
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                cache.CacheMethod(o, value, DelegateHelpers.GetSetMemberDelegate(field));
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                cache.CacheMethod(o, value, DelegateHelpers.GetSetMemberDelegate(property));
                property.SetValue(o, value, null);
                return value;
            }
            throw new ArgumentException($"Could not set member `{MemberName}` on target {o.ToString()}, no such member exists.");
        }
    }

    // (.name o)
    public class CallSiteZeroArityMember
    {
        string MemberName;
        CallSiteCache01 cache;

        public CallSiteZeroArityMember(string memberName)
        {
            MemberName = memberName;
            cache = new CallSiteCache01();
        }

        public object Invoke(object o)
        {
            var sw = System.Diagnostics.Stopwatch.StartNew();
            if(cache.TryGet(o, out var result)) {
                var v = result(o);
                Console.WriteLine($"[CallSiteZeroArityMember] invoke hot in {sw.Elapsed.TotalMilliseconds}ms");
                return v;
            }
            
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                cache.CacheMethod(o, DelegateHelpers.GetZeroArityDelegate(field));
                var v = field.GetValue(o);
                Console.WriteLine($"[CallSiteZeroArityMember] invoke cold {field} in {sw.Elapsed.TotalMilliseconds}ms");
                return v;
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                cache.CacheMethod(o, DelegateHelpers.GetZeroArityDelegate(property));
                var v = Dispatch.InvokeUnwrappingExceptions(property.GetGetMethod(), o, null);
                Console.WriteLine($"[CallSiteZeroArityMember] invoke cold {property} in {sw.Elapsed.TotalMilliseconds}ms");
                return v;
            }
            var method = oType.GetMethod(MemberName, Type.EmptyTypes);
            if (method != null)
            {
                cache.CacheMethod(o, DelegateHelpers.GetZeroArityDelegate(method));
                var v = Dispatch.InvokeUnwrappingExceptions(method, o, null);
                Console.WriteLine($"[CallSiteZeroArityMember] invoke cold {method} in {sw.ElapsedMilliseconds}ms");
                return v;
            }
            throw new ArgumentException($"Could not invoke zero arity member `{MemberName}` on target {(o == null ? "null" : o.ToString())}.");
        }

    }

    // (.name o x)
    public class CallSiteMethod01
    {
        string MemberName;
        CallSiteCache02 cache;

        public CallSiteMethod01(string memberName)
        {
            MemberName = memberName;
            cache = new CallSiteCache02();
        }

        public object Invoke(object o, object arg1)
        {
            var sw = System.Diagnostics.Stopwatch.StartNew();
            if(cache.TryGet(o, arg1, out var result)) {
                var v = result(o, arg1);
                Console.WriteLine($"[CallSizeMethod01] invoke hot in {sw.Elapsed.TotalMilliseconds}ms");
                return v;
            }

            var method = (MethodInfo)Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Instance, o.GetType(), MemberName, new [] { arg1 });
            if (method != null)
            {
                cache.CacheMethod(o, arg1, DelegateHelpers.GetMethodDelegate02(method));
                var v = Dispatch.InvokeUnwrappingExceptions(method, o, new[] { arg1 });
                Console.WriteLine($"[CallSizeMethod01] invoke cold {method} in {sw.ElapsedMilliseconds}ms");
                return v;
            }
            throw new ArgumentException($"Could not invoke instance member method `{MemberName}` on target {o.ToString()} ({o.GetType()}) with argument types { arg1.GetType() }.");
        }

    }

    // TODO artities
    // public class CallSiteInstanceMethod01
    // {
    //     string MethodName;
    //     CallSiteCache02 cache;

    //     public CallSiteInstanceMethod01(string methodName)
    //     {
    //         MethodName = methodName;
    //         cache = new CallSiteCache02(9);
    //     }

    //     public object Invoke(object self, object arg)
    //     {
    //         if (cache.TryGet(self, arg, out var result))
    //             return result(self, arg);
    //         var selfType = self.GetType();
    //         var method = (MethodInfo)Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Instance, selfType, MethodName, new object[] { arg });
    //         if (method != null)
    //         {
    //             Console.WriteLine("[CallSiteInstanceMethod01::Invoke] {0} {1} {2}", self, arg, method);
    //             var func = DelegateHelpers.GetDelegateWeak02(method);
    //             cache.CacheMethod(self, arg, func);
    //             return func(self, arg);
    //         }

    //         throw new Exception($"Could not invoke instance method `{MethodName}` on target { selfType }.");
    //     }

    // }

    // public class CallSiteStaticMethod01
    // {
    //     Type TargetType;
    //     string MethodName;
    //     CallSiteCache01 cache;

    //     public CallSiteStaticMethod01(Type targetType, string methodName)
    //     {
    //         TargetType = targetType;
    //         MethodName = methodName;
    //         // TODO decide between hardcoding cache size and making cache big enough for possible methods
    //         // targetType.GetMethods(BindingFlags.Public | BindingFlags.Static).Where(m => m.Name == methodName).Count()
    //         cache = new CallSiteCache01(9);
    //     }

    //     public object Invoke(object arg)
    //     {
    //         if (cache.TryGet(arg, out var result))
    //             return result(arg);
    //         var method = (MethodInfo)Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Static, TargetType, MethodName, new object[] { arg });
    //         if (method != null)
    //         {
    //             var func = DelegateHelpers.GetDelegateWeak01(method);
    //             cache.CacheMethod(arg, func);
    //             return func(arg);
    //         }

    //         throw new Exception($"Could not invoke static member method `{MethodName}` on target {TargetType} with argument types { arg.GetType() }.");
    //     }
    // }

    // // TODO artities
    // public class CallSiteConstructor
    // {
    //     Type TargetType;

    //     public CallSiteConstructor(Type targetType)
    //     {
    //         TargetType = targetType;
    //     }

    //     public object Invoke(object[] args)
    //     {
    //         return Activator.CreateInstance(TargetType, args);
    //     }
    // }

    // public class CallSiteCache01
    // {
    //     struct Signature
    //     {
    //         Type a;
    //         public Signature(object arg)
    //         {
    //             a = arg.GetType();
    //         }

    //         public bool Match(object arg)
    //         {
    //             return a == arg.GetType();
    //         }
    //     }

    //     struct Entry
    //     {
    //         public Signature Signature;
    //         public Func<object, object> Function;
    //     }
    //     int cacheSize;
    //     int count = 0;

    //     // l0l1Cache[0] is l0 cache, first entry checked
    //     // l0l1Cache[1..cacheSize] is l1 cache, looped through to find best match
    //     Entry[] l0l1Cache;

    //     public CallSiteCache01(int cacheSize)
    //     {
    //         this.cacheSize = cacheSize;
    //         l0l1Cache = new Entry[cacheSize];
    //     }

    //     public CallSiteCache01()
    //     {
    //         this.cacheSize = 9;
    //         l0l1Cache = new Entry[cacheSize];
    //     }

    //     public void CacheMethod(object arg, Func<object, object> func)
    //     {
    //         // Console.WriteLine("[CallSiteCache01] CacheMethod {0} {1} {2}", arg, count, cacheSize);
    //         var c = count < cacheSize ? count++ : count - 1;
    //         l0l1Cache[c] = new Entry { Signature = new Signature(arg), Function = func };
    //         // CacheSwap(0, c);
    //         var temp = l0l1Cache[c];
    //         l0l1Cache[c] = l0l1Cache[0];
    //         l0l1Cache[0] = temp;
    //     }

    //     public bool TryGet(object arg, out Func<object, object> result)
    //     {
    //         for (var i = 0; i < count; i++)
    //         {
    //             var sig = l0l1Cache[i].Signature;
    //             var func = l0l1Cache[i].Function;
    //             if (sig.Match(arg))
    //             {
    //                 // Console.WriteLine($"[CallSiteCache01] cache hit at depth {i}");
    //                 // CacheSwap(0, i);
    //                 var temp = l0l1Cache[i];
    //                 l0l1Cache[i] = l0l1Cache[0];
    //                 l0l1Cache[0] = temp;
    //                 result = func;
    //                 return true;
    //             }
    //         }
    //         // Console.WriteLine($"[CallSiteCache01] cache miss at depth {count}");
    //         result = default;
    //         return false;
    //     }
    // }

    // public class CallSiteCache02
    // {
    //     struct Signature
    //     {
    //         Type a;
    //         Type b;
    //         public Signature(object arg0, object arg1)
    //         {
    //             a = arg0.GetType();
    //             b = arg1.GetType();
    //         }

    //         public bool Match(object arg0, object arg1)
    //         {
    //             return a == arg0.GetType() && b == arg1.GetType();
    //         }
    //     }

    //     struct Entry
    //     {
    //         public Signature Signature;
    //         public Func<object, object, object> Function;
    //     }
    //     int cacheSize;
    //     int count = 0;

    //     // l0l1Cache[0] is l0 cache, first entry checked
    //     // l0l1Cache[1..cacheSize] is l1 cache, looped through to find best match
    //     Entry[] l0l1Cache;

    //     public CallSiteCache02(int cacheSize)
    //     {
    //         this.cacheSize = cacheSize;
    //         l0l1Cache = new Entry[cacheSize];
    //     }

    //     public CallSiteCache02()
    //     {
    //         this.cacheSize = 9;
    //         l0l1Cache = new Entry[cacheSize];
    //     }

    //     public void CacheMethod(object arg0, object arg1, Func<object, object, object> func)
    //     {
    //         var c = count < cacheSize ? count++ : count - 1;
    //         l0l1Cache[c] = new Entry { Signature = new Signature(arg0, arg1), Function = func };
    //         // CacheSwap(0, c);
    //         var temp = l0l1Cache[c];
    //         l0l1Cache[c] = l0l1Cache[0];
    //         l0l1Cache[0] = temp;
    //     }

    //     public bool TryGet(object arg0, object arg1, out Func<object, object, object> result)
    //     {
    //         for (var i = 0; i < count; i++)
    //         {
    //             var sig = l0l1Cache[i].Signature;
    //             var func = l0l1Cache[i].Function;
    //             if (sig.Match(arg0, arg1))
    //             {
    //                 // CacheSwap(0, i);
    //                 var temp = l0l1Cache[i];
    //                 l0l1Cache[i] = l0l1Cache[0];
    //                 l0l1Cache[0] = temp;
    //                 result = func;
    //                 return true;
    //             }
    //         }
    //         result = default;
    //         return false;
    //     }
    // }
}