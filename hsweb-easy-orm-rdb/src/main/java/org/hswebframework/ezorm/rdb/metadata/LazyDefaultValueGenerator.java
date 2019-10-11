package org.hswebframework.ezorm.rdb.metadata;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.core.DefaultValueGenerator;
import org.hswebframework.ezorm.core.RuntimeDefaultValue;

import java.util.function.Supplier;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class LazyDefaultValueGenerator implements DefaultValueGenerator {

    private Supplier<DefaultValueGenerator> generator;

    public static LazyDefaultValueGenerator of(Supplier<DefaultValueGenerator> generator) {
        LazyDefaultValueGenerator valueGenerator = new LazyDefaultValueGenerator();

        valueGenerator.generator = generator;
        return valueGenerator;
    }

    private volatile DefaultValue defaultValue;

    @Override
    public String getSortId() {
        return generator.get().getSortId();
    }

    @Override
    public RuntimeDefaultValue generate() {

        return () -> {
            if (defaultValue == null) {
                defaultValue = generator.get().generate();
            }
            return defaultValue.get();
        };
    }

    @Override
    public String getName() {
        return generator.get().getName();
    }
}
