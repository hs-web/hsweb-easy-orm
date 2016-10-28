/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.core;


import java.util.List;

/**
 * 对象包装器，在执行查询时，通过包装器对查询结果进行初始化
 *
 * @author zhouhao
 * @since 1.0
 */
public interface ObjectWrapper<T> {
    /**
     * 执行初始化,在sql执行后,包装结果前,将调用此方法,传入查询的列
     *
     * @param columns 列集合
     */
    default void setUp(List<String> columns) {
    }

    <C extends T> Class<C> getType();

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
