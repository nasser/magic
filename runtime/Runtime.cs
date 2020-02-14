using System;

namespace Magic
{
    public static class Dispatch
    {
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
            // this binds too tight, does not eg convert int to long
            var argumentTypes = new Type[args.Length];
            for (int i = 0; i < args.Length; i++)
                argumentTypes[i] = args[i].GetType();
            var method = o.GetType().GetMethod(name, argumentTypes);
            if (method != null)
                return method.Invoke(o, args);
            throw new Exception($"Could not invoke member method `{name}` on target {o.ToString()} with argument types { String.Join<Type>(", ", argumentTypes) }.");
        }
    }
}