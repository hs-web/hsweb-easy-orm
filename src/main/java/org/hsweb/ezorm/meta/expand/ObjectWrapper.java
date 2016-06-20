package org.hsweb.ezorm.meta.expand;


import java.util.List;

/**
 * 对象包装器，在执行查询时，通过包装器对查询结果进行初始化
 * Created by 浩 on 2015-11-09 0009.
 */
public interface ObjectWrapper<T> {
    default void setUp(List<String> columns) {
    }

    /**
     * 创建对象实例
     *
     * @return 对象实例
     */
    T newInstance();

    /**
     * 向实例中填充一个属性值
     *
     * @param instance 实例对象
     * @param index    当前实例的索引
     * @param attr     属性名称
     * @param value    属性值
     */
    void wrapper(T instance, int index, String attr, Object value);

    /**
     * 当一个实例被填充完成后调用，已进行其他操作
     *
     * @param instance 实例对象
     */
    void done(T instance);


}
