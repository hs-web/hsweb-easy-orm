package org.hswebframework.ezorm.rdb.executor;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public abstract class SqlRequests {

    /**
     * 基于SQL字符和参数构造SQL请求
     * <p>
     * 如果sql中有占位符表达式{@code #{ }},则使用模版方式进行创建{@link SqlRequests#template(String, Object)}
     * <p>
     * 否则使用预编译方式创建{@link SqlRequests#prepare(String, Object...)}
     * <p>
     * 也支持使用模版进行创建{@link SqlRequests#template(String, Object)}
     *
     * @param sql        SQL
     * @param parameters 参数
     * @return SqlRequest
     */
    public static SqlRequest of(String sql, Object... parameters) {
        if (sql.contains("#{")) {
            return template(sql, parameters.length == 1 ? parameters[0] : parameters);
        }
        return prepare(sql, parameters);
    }

    /**
     * 使用预编译参数创建SQL请求
     * <pre>{@code
     *     //错误的用法
     *     of("select * from order where id = '"+id+"'");
     *     //正确的用法
     *     of("select * from order where id = ?",id);
     * }</pre>
     *
     * @param sql        SQL
     * @param parameters 参数
     * @return SqlRequest
     */
    public static SqlRequest prepare(String sql, Object... parameters) {
        return PrepareSqlRequest.of(sql, parameters);
    }

    /**
     * 使用SQL模版进行创建SQL请求
     * <pre>{@code
     *  template("select * from order where id = #{id}",{id:test})
     * }</pre>
     *
     * @param template  模版
     * @param parameter 参数
     * @return SqlRequest
     */
    public static SqlRequest template(String template, Object parameter) {
        return SqlTemplateParser.parse(template, parameter);
    }
}
