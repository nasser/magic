using System;
using System.Reflection;
using System.Linq;

namespace Magic
{
    public static class Runtime
    {
        public static void InvokeInitType(Assembly assy, Type initType)
        {
            try
            {
                initType.InvokeMember("Initialize", BindingFlags.InvokeMethod | BindingFlags.Static | BindingFlags.Public, Type.DefaultBinder, null, new object[0]);
            }
            catch (Exception e)
            {
                throw new TypeLoadException(String.Format("Error initializing {0}: {1}", assy.FullName, e.Message), e);
            }
        }

        public static string ClojureCLRInitClassName(string sourcePath)
        {
            return "__Init__$" + sourcePath.Replace(".", "/").Replace("/", "$");
        }

        public static bool TryLoadInitType(string relativePath)
        {
            var initClassName = ClojureCLRInitClassName(relativePath);
            Type initType = null;
            foreach (var asm in AppDomain.CurrentDomain.GetAssemblies())
            {
                if (asm.IsDynamic)
                    continue;
                initType = asm.GetType(initClassName);
                if (initType != null)
                    break;
            }
            if (initType == null)
                return false;

            InvokeInitType(initType.Assembly, initType);
            return true;
        }

        // exists to conform with IL2CPP assumptions about a clean stack after a throw
        public static void ThrowHelper(Exception e)
        {
            throw e;
        }

#if CSHARP8
        public static Type? FindType(string p)
#else
        public static Type FindType(string p)
#endif
        {
#if CSHARP8
            Type? t = null;
#else
            Type t = null;
#endif
            // fastest path, will succeed for assembly qualified names (returned by Type.AssemblyQualifiedName)
            // or namespace qualified names (returned by Type.FullName) in the executing assembly or mscorlib
            // e.g. "UnityEngine.Transform, UnityEngine, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
            t = Type.GetType(p, false);

            if (t != null)
                return t;

            AppDomain domain = AppDomain.CurrentDomain;
            Assembly[] assys = domain.GetAssemblies();

            // fast path, will succeed for namespace qualified names (returned by Type.FullName)
            // e.g. "UnityEngine.Transform"
            for(int i = assys.Length - 1; i>=0; i--)
            {
                var assy = assys[i];
                Type t1 = assy.GetType(p, false);
                if(t1 != null)
                      return t1;
            }

            // TODO parse name for complex types
            // if (t == null && p.IndexOfAny(_triggerTypeChars) != -1)
            //     t = ClrTypeSpec.GetTypeFromName(p);

            return t;
        }
    }
    public static class Dispatch
    {

#if CSHARP8
        static object InvokeUnwrappingExceptions(MethodBase method, object? target, object[]? args)
#else
        static object InvokeUnwrappingExceptions(MethodBase method, object target, object[]? args)
#endif
        {
            try {
                return method.Invoke(target, args);
            } catch (TargetInvocationException e) {
                throw e.InnerException;
            }
        }

        // (set! (.name o) value)
        public static object SetMember(object o, string name, object value)
        {
            var oType = o.GetType();
            var valueType = value.GetType();
            var field = oType.GetField(name);
            if (field != null)
            {
                if(field.FieldType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, field.FieldType);
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(name);
            if (property != null)
            {
                if(property.PropertyType.IsPrimitive && valueType.IsPrimitive)
                    value = Convert.ChangeType(value, property.PropertyType);
                property.SetValue(o, value);
                return value;
            }
            throw new Exception($"Could not set member `{name}` on target {o.ToString()}, no such member exists.");
        }
        
        // (.name o)
        public static object InvokeZeroArityMember(object o, string name)
        {
            var oType = o == null ? typeof(Object) : o.GetType();
            var field = oType.GetField(name);
            if (field != null)
                return field.GetValue(o);
            var property = oType.GetProperty(name);
            if (property != null)
                return InvokeUnwrappingExceptions(property.GetMethod, o, null);
            var method = oType.GetMethod(name, Type.EmptyTypes);
            if (method != null)
                return InvokeUnwrappingExceptions(method, o, null);
            throw new Exception($"Could not invoke zero arity member `{name}` on target {(o == null ? "null" : o.ToString())}.");
        }

        static MethodBase? BindToMethod(BindingFlags bindingFlags, Type t, string name, object[] args)
        {
            var methods = t.GetMethods().Where(m => m.Name == name).ToArray();
            Object state;
#if CSHARP8
            MethodBase? method;
#else
            MethodBase method;
#endif
            method = null;
            if(methods.Length > 0)
                 method = Binder.Shared.BindToMethod(bindingFlags,methods,ref args,null,null,null,out state);
            return method;            
        }

        public static object InvokeInstanceMethod(object o, string name, object[] args)
        {
            var method = BindToMethod(BindingFlags.Public | BindingFlags.Instance, o.GetType(), name, args);
            if(method != null)
                return InvokeUnwrappingExceptions(method, o, args);
            throw new Exception($"Could not invoke instance member method `{name}` on target {o.ToString()} ({o.GetType()}) with argument types { String.Join<Object>(", ", args) }.");
        }

        public static object InvokeStaticMethod(object t, string name, object[] args)
        {
            var method = BindToMethod(BindingFlags.Public | BindingFlags.Static, (Type)t, name, args);
            if(method != null)
                return InvokeUnwrappingExceptions(method, null, args);
            throw new Exception($"Could not invoke static member method `{name}` on target {t} with argument types { String.Join<Object>(", ", args) }.");
        }

        public static object InvokeConstructor(Type t, object[] args)
        {
            return Activator.CreateInstance(t, args);
        }
    }
}