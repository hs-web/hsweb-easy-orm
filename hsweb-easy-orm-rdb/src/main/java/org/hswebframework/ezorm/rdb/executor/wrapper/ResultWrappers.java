package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

@NoArgsConstructor(access = AccessLevel.PACKAGE)
public abstract class ResultWrappers {

    public static <E, R> ResultWrapper<E, R> lowerCase(ResultWrapper<E, R> wrapper) {
        return LowerCaseColumnResultWrapper.of(wrapper);
    }

    public static ResultWrapper<Map<String, Object>, Map<String, Object>> map() {
        return MapResultWrapper.defaultInstance();
    }

    public static ResultWrapper<Map<String, Object>, List<Map<String, Object>>> mapList() {
        return list(MapResultWrapper.defaultInstance());
    }

    public static ResultWrapper<Map<String, Object>, Stream<Map<String, Object>>> mapStream() {
        return stream(MapResultWrapper.defaultInstance());
    }

    public static ResultWrapper<Map<String, Object>, Integer> mapConsumer(Consumer<Map<String, Object>> consumer) {
        return consumer(MapResultWrapper.defaultInstance(), consumer);
    }

    public static <E> ResultWrapper<E, List<E>> list(ResultWrapper<E, ?> wrapper) {
        return ListResultWrapper.arrayList(wrapper);
    }

    public static <E> ResultWrapper<E, Stream<E>> stream(ResultWrapper<E, ?> wrapper) {
        return convert(ListResultWrapper.arrayList(wrapper), List::stream);
    }

    public static <E> ResultWrapper<E, Integer> consumer(ResultWrapper<E, ?> wrapper, Consumer<E> consumer) {
        return new ConsumerResultWrapper<>(wrapper, consumer);
    }

    public static <E> ResultWrapper<E, Integer> consumer(ResultWrapper<E, ?> wrapper, Consumer<E> consumer, Runnable onCompleted) {
        return new ConsumerResultWrapper<>(wrapper, consumer, onCompleted);
    }


    public static <E, R, C> ResultWrapper<E, C> convert(ResultWrapper<E, R> wrapper, Function<R, C> converter) {
        return new ConvertResultWrapper<>(wrapper, converter);
    }

    public static ResultWrapper<Map<String, Object>, Map<String, Object>> singleMap() {
        return new SingleResultWrapper<>(MapResultWrapper.defaultInstance());
    }
}
