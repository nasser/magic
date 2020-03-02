using System;
using System.Reflection;
using System.Linq;

namespace Magic
{
    public static class Dispatch
    {
        // (set! (.name o) value)
        public static object SetMember(object o, string name, object value)
        {
            var oType = o.GetType();
            var field = oType.GetField(name);
            if (field != null)
            {
                field.SetValue(o, value);
                return value;
            }
            var property = oType.GetProperty(name);
            if (property != null)
            {
                property.SetValue(o, value);
                return value;
            }
            throw new Exception($"Could not set member `{name}` on target {o.ToString()}, no such member exists.");
        }
        
        // (.name o)
        public static object InvokeZeroArityMember(object o, string name)
        {
            var oType = o.GetType();
            var field = oType.GetField(name);
            if (field != null)
                return field.GetValue(o);
            var property = oType.GetProperty(name);
            if (property != null)
                return property.GetMethod.Invoke(o, null);
            var method = oType.GetMethod(name, Type.EmptyTypes);
            if (method != null)
                return method.Invoke(o, null);
            throw new Exception($"Could not invoke zero arity member `{name}` on target {o.ToString()}.");
        }

        // (.name o args)
        public static object InvokeMember(object o, string name, object[] args)
        {
            var argumentTypes = new Type[args.Length];
            for (int i = 0; i < args.Length; i++)
                argumentTypes[i] = args[i].GetType();
            // var method = o.GetType().GetMethod(name, argumentTypes);
            var methods = o.GetType().GetMethods().Where(m => m.Name == name).ToArray();
            Object state;
            var method = Binder.Shared.BindToMethod(BindingFlags.Public|BindingFlags.Instance,methods,ref args,null,null,null,out state);
            if (method != null)
                return method.Invoke(o, args);
            throw new Exception($"Could not invoke member method `{name}` on target {o.ToString()} ({o.GetType()}) with argument types { String.Join<Type>(", ", argumentTypes) }.");
        }
    }
}