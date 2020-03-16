package org.hswebframework.ezorm.rdb.utils;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.FeatureSupportedMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.function.Function;
import java.util.function.Supplier;

public class ExceptionUtils {

    public static Throwable translation(FeatureSupportedMetadata metadata, Throwable e) {
        return metadata.findFeature(ExceptionTranslation.ID)
                .map(trans -> trans.translate(e))
                .orElse(e);
    }

    @SneakyThrows
    public static <T> T translation(Supplier<T> supplier, FeatureSupportedMetadata metadata) {
        try {
            return supplier.get();
        } catch (Throwable r) {
            throw translation(metadata, r);
        }
    }


    @SneakyThrows
    @SuppressWarnings("all")
    public static <T, P extends Publisher<T>> Function<P, P> translation(FeatureSupportedMetadata metadata) {
        return publisher -> {
            if (publisher instanceof Mono) {
                return (P)((Mono<T>) publisher).onErrorMap(err -> translation(metadata, err));
            }
            if (publisher instanceof Flux) {
                return (P)((Flux<T>) publisher).onErrorMap(err -> translation(metadata, err));
            }
            return publisher;
        };
    }

}
