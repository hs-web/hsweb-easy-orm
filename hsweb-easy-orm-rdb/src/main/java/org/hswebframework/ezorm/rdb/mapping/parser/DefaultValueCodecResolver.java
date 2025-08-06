package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.*;
import org.hswebframework.ezorm.rdb.mapping.EntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.mapping.annotation.Codec;
import org.hswebframework.ezorm.rdb.mapping.annotation.DateTimeCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.EnumCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.JsonCodec;

import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import java.lang.annotation.Annotation;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class DefaultValueCodecResolver implements ValueCodecResolver {

    private final Map<Class<? extends Annotation>, BiFunction<EntityPropertyDescriptor, Annotation, ValueCodec>> annotationStrategies = new ConcurrentHashMap<>();

    private final Map<Class<?>, Function<EntityPropertyDescriptor, ValueCodec>> typeStrategies = new ConcurrentHashMap<>();

    private final Map<Predicate<Class<?>>, Function<EntityPropertyDescriptor, ValueCodec>> predicateStrategies = new ConcurrentHashMap<>();


    public static final DefaultValueCodecResolver COMMONS = new DefaultValueCodecResolver();

    static {
        COMMONS.register(DateTimeCodec.class, (field, ann) -> new org.hswebframework.ezorm.rdb.codec.DateTimeCodec(ann.format(), field.getPropertyType()));

        COMMONS.register(JsonCodec.class, (field, jsonCodec) -> JsonValueCodec.ofField(field.getField()));

        COMMONS.register(EnumCodec.class, (field, jsonCodec) -> new EnumValueCodec(field.getPropertyType(), jsonCodec.property(), jsonCodec.toMask()));
        COMMONS.register(Enumerated.class, (field, jsonCodec) -> new EnumValueCodec(field.getPropertyType(), jsonCodec.value()== EnumType.ORDINAL));

        COMMONS.register(Date.class::isAssignableFrom, field -> new org.hswebframework.ezorm.rdb.codec.DateTimeCodec("yyyy-MM-dd HH:mm:dd", field.getPropertyType()));

        COMMONS.register(Number.class::isAssignableFrom, field -> new NumberValueCodec(field.getPropertyType()));

        COMMONS.register(Boolean.class::isAssignableFrom, field -> new BooleanValueCodec(field.getColumn().getSqlType()));
        COMMONS.register(boolean.class::isAssignableFrom, field -> new BooleanValueCodec(field.getColumn().getSqlType()));

        COMMONS.register(type -> type.isPrimitive() && type != Character.TYPE, field -> new NumberValueCodec(field.getPropertyType()));

        COMMONS.register(Enum.class::isAssignableFrom, field -> new EnumValueCodec(field.getPropertyType()));

        COMMONS.register(Enum[].class::isAssignableFrom, field -> new EnumValueCodec(field.getPropertyType()));


    }

    @SuppressWarnings("all")
    public <T extends Annotation> void register(Class<T> ann, BiFunction<EntityPropertyDescriptor, T, ValueCodec> codecFunction) {
        annotationStrategies.put(ann, (BiFunction) codecFunction);
    }

    public void register(Class<?> ann, Function<EntityPropertyDescriptor, ValueCodec> codecFunction) {
        typeStrategies.put(ann, codecFunction);
    }

    public void register(Predicate<Class<?>> ann, Function<EntityPropertyDescriptor, ValueCodec> codecFunction) {
        predicateStrategies.put(ann, codecFunction);
    }


    @Override
    @SneakyThrows
    public Optional<ValueCodec> resolve(EntityPropertyDescriptor descriptor) {

        Set<Annotation> annotations = descriptor.getAnnotations()
                .stream()
                .filter(ann -> null != ann.annotationType().getAnnotation(Codec.class))
                .collect(Collectors.toSet());

        List<ValueCodec> codecs = new ArrayList<>();

        for (Annotation annotation : annotations) {
            BiFunction<EntityPropertyDescriptor, Annotation, ValueCodec> function = annotationStrategies.get(annotation.annotationType());
            if (function != null) {
                codecs.add(function.apply(descriptor, annotation));
            }
        }
        if (codecs.size() == 1) {
            return Optional.of(codecs.get(0));
        }

        return Optional.ofNullable(typeStrategies.get(descriptor.getPropertyType()))
                .map(func -> func.apply(descriptor))
                .map(Optional::of)
                .orElseGet(() ->
                        predicateStrategies.entrySet().stream()
                                .filter(e -> e.getKey().test(descriptor.getPropertyType()))
                                .map(e -> e.getValue().apply(descriptor))
                                .findFirst());
    }


}
