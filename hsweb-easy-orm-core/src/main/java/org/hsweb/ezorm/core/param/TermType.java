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

package org.hsweb.ezorm.core.param;

/**
 * 提供默认支持的查询条件类型,用于动态指定查询条件
 *
 * @author zhouhao
 * @since 1.0
 */
public interface TermType {
    /**
     * ==
     *
     * @since 1.0
     */
    String eq      = "eq";
    /**
     * !=
     *
     * @since 1.0
     */
    String not     = "not";
    /**
     * like
     *
     * @since 1.0
     */
    String like    = "like";
    /**
     * not like
     *
     * @since 1.0
     */
    String nlike   = "nlike";
    /**
     * >
     *
     * @since 1.0
     */
    String gt      = "gt";
    /**
     * <
     *
     * @since 1.0
     */
    String lt      = "lt";
    /**
     * >=
     *
     * @since 1.0
     */
    String gte     = "gte";
    /**
     * <=
     *
     * @since 1.0
     */
    String lte     = "lte";
    /**
     * in
     *
     * @since 1.0
     */
    String in      = "in";
    /**
     * notin
     *
     * @since 1.0
     */
    String nin     = "nin";
    /**
     * =''
     *
     * @since 1.0
     */
    String empty   = "empty";
    /**
     * !=''
     *
     * @since 1.0
     */
    String nempty  = "nempty";
    /**
     * is null
     *
     * @since 1.0
     */
    String isnull  = "isnull";
    /**
     * not null
     *
     * @since 1.0
     */
    String notnull = "notnull";
    /**
     * between
     *
     * @since 1.0
     */
    String btw     = "btw";
    /**
     * not between
     *
     * @since 1.0
     */
    String nbtw    = "nbtw";

    /**
     * 此类型将直接执行sql.在类型是从客户端参数中获取的场景中,应该屏蔽此类型
     *
     * @see SqlTerm
     * @since 1.0
     * @deprecated 此属性已弃用，如果想直接拼接sql，请使用 {@link SqlTerm}
     */
    @Deprecated
    String func = "func";
}
