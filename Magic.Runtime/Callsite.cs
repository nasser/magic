using System;
using System.Reflection;
using System.Linq;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Magic
{
    public static class DelegateHelpers
    {
        public static T CreateDelegate<T>(MethodInfo method) where T : Delegate
        {
            Console.WriteLine("[CreateDelegate] {0} {1}", method, typeof(T));
            return (T)Delegate.CreateDelegate(typeof(T), null, method);
        }
        public static Func<object, object> GetDelegateWeakFromStrong01<A, Z>(MethodInfo method)
        {
            var func = CreateDelegate<Func<A, Z>>(method);
            Func<object, object> ret = a => func((A)a);
            return ret;
        }

        public static Func<object, object> GetDelegateWeak01(MethodInfo method)
        {
            MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong01), BindingFlags.Public | BindingFlags.Static);
            MethodInfo constructedHelper;
            var parameters = method.GetParameters();
            if(method.IsStatic)
            {
                Debug.Assert(parameters.Length == 1);
                constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, method.ReturnType);
            }
            else
            {
                Debug.Assert(parameters.Length == 0);
                constructedHelper = helper.MakeGenericMethod(method.DeclaringType, method.ReturnType);
            }
            object ret = constructedHelper.Invoke(null, new object[] { method });
            return (Func<object, object>)ret;
        }

        public static Func<object, object, object> GetDelegateWeakFromStrong02<A, B, Z>(MethodInfo method)
        {
            Console.WriteLine("[GetDelegateWeakFromStrong02] {0} {1} {2} {3}", method, typeof(A), typeof(B), typeof(Z));
            var func = CreateDelegate<Func<A, B, Z>>(method);
            Func<object, object, object> ret = (a, b) => func((A)a, (B)b);
            return ret;
        }

        public static Func<object, object, object> GetDelegateWeak02(MethodInfo method)
        {
            MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong02), BindingFlags.Public | BindingFlags.Static);
            MethodInfo constructedHelper;
            var parameters = method.GetParameters();
            if(method.IsStatic)
            {
                Debug.Assert(parameters.Length == 2);
                constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, parameters[1].ParameterType, method.ReturnType);
            }
            else
            {
                Debug.Assert(parameters.Length == 1);
                constructedHelper = helper.MakeGenericMethod(method.DeclaringType, parameters[0].ParameterType, method.ReturnType);
            }
            object ret = constructedHelper.Invoke(null, new object[] { method });
            return (Func<object, object, object>)ret;
        }

        public static Func<object, object, object, object> GetDelegateWeakFromStrong03<A, B, C, Z>(MethodInfo method)
        {
            var func = CreateDelegate<Func<A, B, C, Z>>(method);
            Func<object, object, object, object> ret = (a, b, c) => func((A)a, (B)b, (C)c);
            return ret;
        }

        public static Func<object, object, object, object> GetDelegateWeak03(MethodInfo method)
        {
            MethodInfo helper = typeof(DelegateHelpers).GetMethod(nameof(GetDelegateWeakFromStrong03), BindingFlags.Public | BindingFlags.Static);
            MethodInfo constructedHelper;
            var parameters = method.GetParameters();
            if(method.IsStatic)
            {
                Debug.Assert(parameters.Length == 3);
                constructedHelper = helper.MakeGenericMethod(parameters[0].ParameterType, parameters[1].ParameterType, parameters[2].ParameterType, method.ReturnType);
            }
            else
            {
                Debug.Assert(parameters.Length == 2);
                constructedHelper = helper.MakeGenericMethod(method.DeclaringType, parameters[0].ParameterType, parameters[1].ParameterType, method.ReturnType);
            }
            object ret = constructedHelper.Invoke(null, new object[] { method });
            return (Func<object, object, object, object>)ret;
        }
    }

    // (set! (.name o) value)
    public class CallSiteSetMember
    {
        string MemberName;

        public CallSiteSetMember(string memberName)
        {
            MemberName = memberName;
        }

        public object Invoke(object o, object value)
        {
            var oType = o.GetType();
            var valueType = value.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
            {
                if (field.FieldType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, field.FieldType);
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(MemberName);
            if (property != null)
            {
                if (property.PropertyType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, property.PropertyType);
                property.SetValue(o, value, null);
                return value;
            }
            throw new Exception($"Could not set member `{MemberName}` on target {o.ToString()}, no such member exists.");
        }
    }

    // (.name o)
    public class CallSiteZeroArityMember
    {
        string MemberName;

        public CallSiteZeroArityMember(string memberName)
        {
            MemberName = memberName;
        }

        public object Invoke(object o)
        {
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(MemberName);
            if (field != null)
                return field.GetValue(o);
            var property = oType.GetProperty(MemberName);
            if (property != null)
                return Dispatch.InvokeUnwrappingExceptions(property.GetGetMethod(), o, null);
            var method = oType.GetMethod(MemberName, Type.EmptyTypes);
            if (method != null)
                return Dispatch.InvokeUnwrappingExceptions(method, o, null);
            throw new Exception($"Could not invoke zero arity member `{MemberName}` on target {(o == null ? "null" : o.ToString())}.");
        }

    }

    // TODO artities
    public class CallSiteInstanceMethod01
    {
        string MethodName;
        CallSiteCache02 cache;

        public CallSiteInstanceMethod01(string methodName)
        {
            MethodName = methodName;
            cache = new CallSiteCache02(9);
        }

        public object Invoke(object self, object arg)
        {
            if (cache.TryGet(self, arg, out var result))
                return result(self, arg);
            var selfType = self.GetType();
            var method = (MethodInfo)Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Instance, selfType, MethodName, new object[] { arg });
            if (method != null)
            {
                Console.WriteLine("[CallSiteInstanceMethod01::Invoke] {0} {1} {2}", self, arg, method);
                var func = DelegateHelpers.GetDelegateWeak02(method);
                cache.CacheMethod(self, arg, func);
                return func(self, arg);
            }

            throw new Exception($"Could not invoke instance method `{MethodName}` on target { selfType }.");
        }

    }

    public class CallSiteStaticMethod01
    {
        Type TargetType;
        string MethodName;
        CallSiteCache01 cache;

        public CallSiteStaticMethod01(Type targetType, string methodName)
        {
            TargetType = targetType;
            MethodName = methodName;
            // TODO decide between hardcoding cache size and making cache big enough for possible methods
            // targetType.GetMethods(BindingFlags.Public | BindingFlags.Static).Where(m => m.Name == methodName).Count()
            cache = new CallSiteCache01(9);
        }

        public object Invoke(object arg)
        {
            if (cache.TryGet(arg, out var result))
                return result(arg);
            var method = (MethodInfo)Dispatch.BindToMethod(BindingFlags.Public | BindingFlags.Static, TargetType, MethodName, new object[] { arg });
            if (method != null)
            {
                var func = DelegateHelpers.GetDelegateWeak01(method);
                cache.CacheMethod(arg, func);
                return func(arg);
            }

            throw new Exception($"Could not invoke static member method `{MethodName}` on target {TargetType} with argument types { arg.GetType() }.");
        }
    }

    // TODO artities
    public class CallSiteConstructor
    {
        Type TargetType;

        public CallSiteConstructor(Type targetType)
        {
            TargetType = targetType;
        }

        public object Invoke(object[] args)
        {
            return Activator.CreateInstance(TargetType, args);
        }
    }

    public class CallSiteCache01
    {
        struct Signature
        {
            Type a;
            public Signature(object arg)
            {
                a = arg.GetType();
            }

            public bool Match(object arg)
            {
                return a == arg.GetType();
            }
        }

        struct Entry
        {
            public Signature Signature;
            public Func<object, object> Function;
        }
        int cacheSize;
        int count = 0;

        // l0l1Cache[0] is l0 cache, first entry checked
        // l0l1Cache[1..cacheSize] is l1 cache, looped through to find best match
        Entry[] l0l1Cache;

        public CallSiteCache01(int cacheSize)
        {
            this.cacheSize = cacheSize;
            l0l1Cache = new Entry[cacheSize];
        }

        public CallSiteCache01()
        {
            this.cacheSize = 9;
            l0l1Cache = new Entry[cacheSize];
        }

        public void CacheMethod(object arg, Func<object, object> func)
        {
            var c = count < cacheSize ? count++ : count - 1;
            l0l1Cache[c] = new Entry { Signature = new Signature(arg), Function = func };
            // CacheSwap(0, c);
            var temp = l0l1Cache[c];
            l0l1Cache[c] = l0l1Cache[0];
            l0l1Cache[0] = temp;
        }

        public bool TryGet(object arg, out Func<object, object> result)
        {
            for (var i = 0; i < count; i++)
            {
                var sig = l0l1Cache[i].Signature;
                var func = l0l1Cache[i].Function;
                if (sig.Match(arg))
                {
                    // CacheSwap(0, i);
                    var temp = l0l1Cache[i];
                    l0l1Cache[i] = l0l1Cache[0];
                    l0l1Cache[0] = temp;
                    result = func;
                    return true;
                }
            }
            result = default;
            return false;
        }
    }

    public class CallSiteCache02
    {
        struct Signature
        {
            Type a;
            Type b;
            public Signature(object arg0, object arg1)
            {
                a = arg0.GetType();
                b = arg1.GetType();
            }

            public bool Match(object arg0, object arg1)
            {
                return a == arg0.GetType() && b == arg1.GetType();
            }
        }

        struct Entry
        {
            public Signature Signature;
            public Func<object, object, object> Function;
        }
        int cacheSize;
        int count = 0;

        // l0l1Cache[0] is l0 cache, first entry checked
        // l0l1Cache[1..cacheSize] is l1 cache, looped through to find best match
        Entry[] l0l1Cache;

        public CallSiteCache02(int cacheSize)
        {
            this.cacheSize = cacheSize;
            l0l1Cache = new Entry[cacheSize];
        }

        public CallSiteCache02()
        {
            this.cacheSize = 9;
            l0l1Cache = new Entry[cacheSize];
        }

        public void CacheMethod(object arg0, object arg1, Func<object, object, object> func)
        {
            var c = count < cacheSize ? count++ : count - 1;
            l0l1Cache[c] = new Entry { Signature = new Signature(arg0, arg1), Function = func };
            // CacheSwap(0, c);
            var temp = l0l1Cache[c];
            l0l1Cache[c] = l0l1Cache[0];
            l0l1Cache[0] = temp;
        }

        public bool TryGet(object arg0, object arg1, out Func<object, object, object> result)
        {
            for (var i = 0; i < count; i++)
            {
                var sig = l0l1Cache[i].Signature;
                var func = l0l1Cache[i].Function;
                if (sig.Match(arg0, arg1))
                {
                    // CacheSwap(0, i);
                    var temp = l0l1Cache[i];
                    l0l1Cache[i] = l0l1Cache[0];
                    l0l1Cache[0] = temp;
                    result = func;
                    return true;
                }
            }
            result = default;
            return false;
        }
    }
}