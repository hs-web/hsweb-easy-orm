package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.lang.reflect.Array;

@Getter
@Slf4j
public class SerializedLambda implements Serializable {
    private static final long serialVersionUID = 8025925345765570181L;
    private Class<?> capturingClass;
    private String functionalInterfaceClass;
    private String functionalInterfaceMethodName;
    private String functionalInterfaceMethodSignature;
    private String implClass;
    private String implMethodName;
    private String implMethodSignature;
    private int implMethodKind;
    private String instantiatedMethodType;
    private Object[] capturedArgs;

    private Class<?> implClassResolved;

    @SneakyThrows
    public Class<?> getImplClass() {
        boolean isArr = false;
        if (implClassResolved == null) {
            String className = implClass;
            try {
                String type = getInstantiatedMethodType();
                type = type.substring(2, type.indexOf(";"));
                if (!type.startsWith("L")) {
                    className = type;
                }
                if (className.startsWith("[L")) {
                    className = className.substring(2);
                    isArr = true;
                }
            } catch (Throwable ignore) {

            }
            implClassResolved = Class.forName(className.replace('/', '.'));
        }
        if (isArr) {
            return Array.newInstance(implClassResolved, 0).getClass();
        }
        return implClassResolved;
    }

    public String getMethodName() {
        return implMethodName;
    }

    @SneakyThrows
    public static SerializedLambda of(Object lambdaColumn) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
            ClassLoader loader = lambdaColumn.getClass().getClassLoader();

            try (ObjectOutputStream oos = new ObjectOutputStream(baos)) {
                oos.writeObject(lambdaColumn);
                oos.flush();
                byte[] data = baos.toByteArray();
                try (ObjectInputStream objIn = new ObjectInputStream(new ByteArrayInputStream(data)) {
                    @Override
                    protected Class<?> resolveClass(ObjectStreamClass objectStreamClass) throws IOException, ClassNotFoundException {
                        String name = objectStreamClass.getName();
                        if ("java.lang.invoke.SerializedLambda".equals(name)) {
                            return SerializedLambda.class;
                        }
                        try {
                            return Class.forName(name, false, loader);
                        } catch (Throwable ignore) {
                        }
                        return super.resolveClass(objectStreamClass);
                    }
                }) {
                    SerializedLambda lambda = (SerializedLambda) objIn.readObject();
                    if (lambda.getMethodName().startsWith("lambda$")) {
                        throw new UnsupportedOperationException("请使用方法引用,例如: UserEntity::getName");
                    }
                    return lambda;
                }
            }
        } catch (NotSerializableException e) {

            throw new UnsupportedOperationException("请将类[" + e.getMessage() + "]实现[Serializable]接口");
        }
    }
}
