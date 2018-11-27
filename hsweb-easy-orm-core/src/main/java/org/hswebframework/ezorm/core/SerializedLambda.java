package org.hswebframework.ezorm.core;

import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.io.*;

@Getter
@Slf4j
public class SerializedLambda implements Serializable {
    private static final long     serialVersionUID = 8025925345765570181L;
    private              Class<?> capturingClass;
    private              String   functionalInterfaceClass;
    private              String   functionalInterfaceMethodName;
    private              String   functionalInterfaceMethodSignature;
    private              String   implClass;
    private              String   implMethodName;
    private              String   implMethodSignature;
    private              int      implMethodKind;
    private              String   instantiatedMethodType;
    private              Object[] capturedArgs;

    public String getMethodName() {
        return implMethodName;
    }

    @SneakyThrows
    public static SerializedLambda of(LambdaColumn lambdaColumn) {
        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
            try (ObjectOutputStream oos = new ObjectOutputStream(baos)) {
                oos.writeObject(lambdaColumn);
                oos.flush();
                byte[] data = baos.toByteArray();
                try (ObjectInputStream objIn = new ObjectInputStream(new ByteArrayInputStream(data)) {
                    @Override
                    protected Class<?> resolveClass(ObjectStreamClass objectStreamClass) throws IOException, ClassNotFoundException {
                        Class<?> clazz = super.resolveClass(objectStreamClass);
                        return clazz == java.lang.invoke.SerializedLambda.class ? SerializedLambda.class : clazz;
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
            throw new UnsupportedOperationException("请使用方法引用,例如: UserEntity::getName");
        }
    }
}
