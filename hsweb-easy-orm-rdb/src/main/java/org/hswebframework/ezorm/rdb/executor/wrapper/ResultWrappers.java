package org.hswebframework.ezorm.rdb.executor.wrapper;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.hswebframework.ezorm.core.Decoder;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * 通用查询结果包装器大全
 */
@NoArgsConstructor(access = AccessLevel.PACKAGE)
public abstract class ResultWrappers {

    /**
     * 创建将列转为小写的包装器
     *
     * @param wrapper 下级包装器
     * @param <E>     行类型
     * @param <R>     结果类型
     * @return 小写列包装器
     */
    public static <E, R> ResultWrapper<E, R> lowerCase(ResultWrapper<E, R> wrapper) {
        return LowerCaseColumnResultWrapper.of(wrapper);
    }

    /**
     * 将行转为Map的包装器,此包装器不具备收集能力,通常需要配合具备收集能力的包装器使用,如:
     * <pre>
     *     ResultWrappers.list(map());//多个map结果集
     *
     *     ResultWrappers.single(map());//单个map结果
     * </pre>
     *
     * @return Map结果包装器
     * @see MapResultWrapper
     */
    public static ResultWrapper<Map<String, Object>, Map<String, Object>> map() {
        return MapResultWrapper.defaultInstance();
    }

    /**
     * @return map集合结果包装器
     * @see ResultWrappers#map()
     * @see ResultWrappers#list(ResultWrapper)
     */
    public static ResultWrapper<Map<String, Object>, List<Map<String, Object>>> mapList() {
        return list(map());
    }

    /**
     * @return map流结果包装器
     * @see ResultWrappers#map()
     * @see ResultWrappers#stream(ResultWrapper)
     */
    public static ResultWrapper<Map<String, Object>, Stream<Map<String, Object>>> mapStream() {
        return stream(map());
    }

    /**
     * Set结果包装器
     *
     * @param wrapper 行包装器
     * @param <E>     行结果类型
     * @return Set结果包装器
     */
    public static <E> ResultWrapper<E, Set<E>> set(ResultWrapper<E, ?> wrapper) {
        return ListResultWrapper.of(wrapper, new LinkedHashSet<>());
    }

    /**
     * 创建流结果包装器
     *
     * @param wrapper 流结果
     * @param <E>     行结果类型
     * @return 流结果包装器
     */
    public static <E> ResultWrapper<E, Stream<E>> stream(ResultWrapper<E, ?> wrapper) {
        return convert(list(wrapper), List::stream);
    }

    /**
     * @return 单个map的结果包装器
     * @see ResultWrappers#single(ResultWrapper)
     * @see ResultWrappers#map()
     */
    public static ResultWrapper<Map<String, Object>, Map<String, Object>> singleMap() {
        return single(map());
    }

    /**
     * 集合结果包装器,将所有行结果收集成一个集合
     *
     * @param wrapper 行包装器
     * @param <E>     行结果类型
     * @return 结果集合包装器
     * @see ResultWrappers#mapList()
     */
    public static <E> ResultWrapper<E, List<E>> list(ResultWrapper<E, ?> wrapper) {
        return ListResultWrapper.of(wrapper, new ArrayList<>());
    }

    /**
     * 可选结果包装器
     *
     * @param wrapper 行包装器
     * @param <E>     行结果类型
     * @return 可选结果包装器
     * @see Optional
     * @see ResultWrappers#single(ResultWrapper)
     * @see ResultWrappers#convert(ResultWrapper, Function)
     */
    public static <E, R> ResultWrapper<E, Optional<R>> optional(ResultWrapper<E, R> wrapper) {
        return convert(wrapper, Optional::ofNullable);
    }

    /**
     * 单个结果包装器,结果可能为 <code>null</code>
     *
     * @param wrapper 行包装器
     * @param <E>     行结果类型
     * @return 单个结果包装器
     * @see ResultWrappers#optional(ResultWrapper)
     */
    public static <E> ResultWrapper<E, E> single(ResultWrapper<E, ?> wrapper) {
        return new SingleResultWrapper<>(wrapper);
    }

    /**
     * 创建单列结果包装器,只包装处理单个列的数据
     * <pre>
     *     //只获取id结果集
     *     ResultWrappers.list(column("id",String::valueOf))
     *
     *     //获取单个结果
     *     ResultWrappers.single(column("total",Number.class::cast))
     * </pre>
     *
     * @param column  列名
     * @param decoder 解码器
     * @param <R>     结果类型
     * @return 单个列结果包装器
     */
    public static <R> ResultWrapper<R, R> column(String column, Decoder<R> decoder) {
        return new SingleColumnResultWrapper<>(column, decoder);
    }

    /**
     * 创建不收集结果,只消费行结果的包装器
     *
     * @param wrapper  行结果包装器
     * @param consumer 行结果消费者
     * @param <E>      行类型
     * @return 包装器
     */
    public static <E> ResultWrapper<E, Integer> consumer(ResultWrapper<E, ?> wrapper, Consumer<E> consumer) {
        return new ConsumerResultWrapper<>(wrapper, consumer);
    }

    /**
     * 创建不收集结果,只消费行结果的包装器,并支持全部消费完后,执行指定的任务
     *
     * @param wrapper     行结果包装器
     * @param consumer    行结果消费者
     * @param onCompleted 当全部执行完成后执行的任务
     * @param <E>         行类型
     * @return 包装器
     */
    public static <E> ResultWrapper<E, Integer> consumer(ResultWrapper<E, ?> wrapper, Consumer<E> consumer, Runnable onCompleted) {
        return new ConsumerResultWrapper<>(wrapper, consumer, onCompleted);
    }

    /**
     * 创建转换结果的包装器
     *
     * @param wrapper   原始包装器
     * @param converter 结果转换器
     * @param <E>       行类型
     * @param <R>       原始结果类型
     * @param <C>       转换后的结果类型
     * @return 结果包装器
     */
    public static <E, R, C> ResultWrapper<E, C> convert(ResultWrapper<E, R> wrapper, Function<R, C> converter) {
        return new ConvertResultWrapper<>(wrapper, converter);
    }

}
