package org.hswebframework.ezorm.rdb.metadata;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;
import org.hswebframework.ezorm.core.meta.ObjectMetadata;

import java.util.function.Supplier;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LazyDefaultValueGenerator<T extends ObjectMetadata> implements DefaultValueGenerator<T > {

    private Supplier<DefaultValueGenerator<T>> generator;

    public static <T extends ObjectMetadata> LazyDefaultValueGenerator<T> of(Supplier<DefaultValueGenerator<T>> generator) {
        LazyDefaultValueGenerator<T> valueGenerator = new LazyDefaultValueGenerator<>();

        valueGenerator.generator = generator;
        return valueGenerator;
    }

    private volatile DefaultValue defaultValue;

    @Override
    public String getSortId() {
        return generator.get().getSortId();
    }

    @Override
    public RuntimeDefaultValue generate(T meta) {

        return () -> {
            if (defaultValue == null) {
                defaultValue = generator.get().generate(meta);
            }
            return defaultValue.get();
        };
    }

    @Override
    public String getName() {
        return generator.get().getName();
    }
}
