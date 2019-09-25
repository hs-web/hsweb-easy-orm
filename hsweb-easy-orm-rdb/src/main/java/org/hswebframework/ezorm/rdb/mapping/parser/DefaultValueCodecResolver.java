package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.codec.NumberValueCodec;
import org.hswebframework.ezorm.rdb.mapping.annotation.Codec;
import org.hswebframework.ezorm.rdb.mapping.annotation.DateTimeCodec;
import org.hswebframework.ezorm.rdb.utils.AnnotationUtils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Date;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class DefaultValueCodecResolver implements ValueCodecResolver {

    private Map<Class<? extends Annotation>, BiFunction<Field, Annotation, ValueCodec>>
            annotationStrategies = new ConcurrentHashMap<>();

    private Map<Class, Function<Field, ValueCodec>> typeStrategies = new ConcurrentHashMap<>();

    private Map<Predicate<Class>, Function<Field, ValueCodec>> predicateStrategies = new ConcurrentHashMap<>();


    public static final DefaultValueCodecResolver COMMONS = new DefaultValueCodecResolver();

    static {
        COMMONS.register(DateTimeCodec.class,
                (field, ann) -> new org.hswebframework.ezorm.rdb.codec.DateTimeCodec(ann.format(), field.getType()));
        COMMONS.register(Date.class::isAssignableFrom, field -> new org.hswebframework.ezorm.rdb.codec.DateTimeCodec("yyyy-MM-dd HH", field.getType()));

        COMMONS.register(Number.class::isAssignableFrom, field ->new NumberValueCodec(field.getType()));


    }

    @SuppressWarnings("all")
    public <T extends Annotation> void register(Class<T> ann, BiFunction<Field, T, ValueCodec> codecFunction) {
        annotationStrategies.put(ann, (BiFunction) codecFunction);
    }

    public void register(Class ann, Function<Field, ValueCodec> codecFunction) {
        typeStrategies.put(ann, (Function) codecFunction);
    }

    public void register(Predicate<Class> ann, Function<Field, ValueCodec> codecFunction) {
        predicateStrategies.put(ann, (Function) codecFunction);
    }


    @Override
    @SneakyThrows
    public Optional<ValueCodec> resolve(Class entityType, PropertyDescriptor descriptor) {

        Set<Annotation> annotations = AnnotationUtils.getAnnotations(entityType, descriptor)
                .stream()
                .filter(ann -> null != ann.annotationType().getAnnotation(Codec.class))
                .collect(Collectors.toSet());

        Field field = entityType.getDeclaredField(descriptor.getName());

        for (Annotation annotation : annotations) {
            BiFunction<Field, Annotation, ValueCodec> function = annotationStrategies.get(annotation.annotationType());
            if (function != null) {
                return Optional.of(function.apply(field, annotation));
            }
        }
        return Optional.ofNullable(typeStrategies.get(field.getType()))
                .map(func -> func.apply(field))
                .map(Optional::of)
                .orElseGet(() ->
                        predicateStrategies.entrySet().stream()
                                .filter(e -> e.getKey().test(field.getType()))
                                .map(e -> e.getValue().apply(field))
                                .findFirst());
    }


}
