using System;
using System.Reflection;
using System.Linq;

namespace Magic
{
    public static class Runtime
    {

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
            foreach (Assembly assy in assys)
            {
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
                return property.GetMethod.Invoke(o, null);
            var method = oType.GetMethod(name, Type.EmptyTypes);
            if (method != null)
                return method.Invoke(o, null);
            throw new Exception($"Could not invoke zero arity member `{name}` on target {(o == null ? "null" : o.ToString())}.");
        }

        // (.name o args)
        public static object InvokeMember(object o, string name, object[] args)
        {
            var argumentTypes = new Type[args.Length];
            for (int i = 0; i < args.Length; i++)
                argumentTypes[i] = args[i] == null ? typeof(Object) : args[i].GetType();
            // var method = o.GetType().GetMethod(name, argumentTypes);
            var methods = o.GetType().GetMethods().Where(m => m.Name == name).ToArray();
            Object state;
            var method = Binder.Shared.BindToMethod(BindingFlags.Public|BindingFlags.Instance,methods,ref args,null,null,null,out state);
            if (method != null)
                return method.Invoke(o, args);
            throw new Exception($"Could not invoke member method `{name}` on target {o.ToString()} ({o.GetType()}) with argument types { String.Join<Type>(", ", argumentTypes) }.");
        }

        public static object InvokeConstructor(Type t, object[] args)
        {
            return Activator.CreateInstance(t, args);
        }
    }
}