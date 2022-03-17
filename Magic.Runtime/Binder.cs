using System;
using System.Linq;
using System.Globalization;
using System.Reflection;

namespace Magic
{
    public class Binder : System.Reflection.Binder
    {
        public static readonly Binder Shared = new Binder();

        System.Reflection.Binder _binder = Type.DefaultBinder;

        public override FieldInfo BindToField(BindingFlags bindingAttr, FieldInfo[] match, object value, CultureInfo culture)
        {
            return _binder.BindToField(bindingAttr, match, value, culture);
        }

        public object ConvertArgument(Type type, object argument)
        {
            if(type.IsEnum)
                return Enum.ToObject(type, argument);

            else if(argument is IConvertible)
                return Convert.ChangeType(argument, type);

            return argument;
        }

        public void ConvertArguments(MethodBase method, object[] args)
        {
            var parameters = method.GetParameters();
            for (int i = 0; i < args.Length; i++)
            {
                args[i] = ConvertArgument(parameters[i].ParameterType, args[i]);
            }
        }

        public override MethodBase BindToMethod(BindingFlags bindingAttr, MethodBase[] match, ref object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] names, out object state)
        {
            try {
                var nativeResult = _binder.BindToMethod(bindingAttr, match, ref args, modifiers, culture, names, out state);
                if(nativeResult != null)
                    return nativeResult;
            } catch(MissingMemberException) {
                // ignore
            }
            var argumentTypes = args.Select(a => a == null ? typeof(Object) : a.GetType()).ToArray();
            state = new Object(); // ???
            var result = SelectMethod(bindingAttr, match, argumentTypes, modifiers);
            if(result != null)
            {
                ConvertArguments(result, args);
            }

            return result;
        }

        public override object ChangeType(object value, Type type, CultureInfo culture)
        {
            return _binder.ChangeType(value, type, culture);
        }

        public override void ReorderArgumentArray(ref object[] args, object state)
        {
            _binder.ReorderArgumentArray(ref args, state);
        }

        public override MethodBase SelectMethod(BindingFlags bindingAttr, MethodBase[] match, Type[] argumentTypes, ParameterModifier[] modifiers)
        {
            if(match.Length == 0)
                return null;

            for (int i = 0; i < match.Length; i++)
                if(match[i] == null)
                    return null;

            for (int i = 0; i < argumentTypes.Length; i++)
                if(argumentTypes[i] == null)
                    return null;
            
            MethodBase result = null;
            // SelectMethod on the default binder throws an exception if passed
            // typebuilders. this comes up when trying to bind to proxy methods
            // during analysis when (the proxy type is not yet complete). we
            // skip the default binder in this case and fall back on our own logic
            bool allRuntimeTypes = true;
            for (var i=0; i<argumentTypes.Length; i++) {
                if (argumentTypes[i] is System.Reflection.Emit.TypeBuilder) {
                    allRuntimeTypes = false;
                    break;
                }
            }
            if(allRuntimeTypes)
                result = _binder.SelectMethod(bindingAttr, match, argumentTypes, modifiers);
            if (result != null)
                return result;
            foreach (var candidate in match)
            {
                if (MatchByMagicBindingRules(bindingAttr, candidate, argumentTypes, modifiers))
                {
                    if (result != null)
                        throw new AmbiguousMatchException();
                    else
                    {
                        result = candidate;
                        continue;
                    }
                }
            }

            return result;
        }

        // https://stackoverflow.com/questions/31173612/type-getenumunderlyingtype-replacement-for-net35
        static Type GetEnumUnderlyingType(Type t)
        {
            if (!t.IsEnum)
                throw new ArgumentException("Expected Enum type, got " + t.FullName);
            FieldInfo[] fields = t.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            if (fields == null || fields.Length != 1)
                throw new ArgumentException("Enum invalid");
            return fields[0].FieldType;
        }

        static bool MatchesByEnumConversionConversion(Type argumentType, Type parameterType)
            => parameterType.IsEnum
               && GetEnumUnderlyingType(parameterType) == argumentType;

        static bool MatchesByObjectConversion(Type argumentType, Type parameterType)
            => argumentType == typeof(object)
               || parameterType.IsAssignableFrom(argumentType);

        static bool MatchesByNarrowingConversion(Type argumentType, Type parameterType)
            => parameterType == argumentType
               || (parameterType.IsPrimitive && argumentType.IsPrimitive && parameterType != typeof(Boolean) && argumentType != typeof(Boolean))
               || parameterType.IsAssignableFrom(argumentType);

        static bool MatchesByInterfaceConversion(Type argumentType, Type parameterType) 
            => parameterType.IsInterface && argumentType.IsInterface;

        static bool MatchByMagicBindingRules(BindingFlags bindingAttr, MethodBase candidate, Type[] argumentTypes, ParameterModifier[] modifiers)
        {
            var parameters = candidate.GetParameters();
            if (parameters.Length != argumentTypes.Length) return false;
            for (int i = 0; i < parameters.Length; i++)
            {
                var parameterType = parameters[i].ParameterType;
                var argumentType = argumentTypes[i];
                if(MatchesByNarrowingConversion(argumentType, parameterType)
                   || MatchesByEnumConversionConversion(argumentType, parameterType)
                   || MatchesByObjectConversion(argumentType, parameterType)
                   || MatchesByInterfaceConversion(argumentType, parameterType))
                {
                    continue;
                }
                else 
                {
                    return false;
                }
            }
            return true;
        }
        public override PropertyInfo SelectProperty(BindingFlags bindingAttr, PropertyInfo[] match, Type returnType, Type[] indexes, ParameterModifier[] modifiers)
        {
            return _binder.SelectProperty(bindingAttr, match, returnType, indexes, modifiers);
        }
    }
}