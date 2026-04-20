package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.BatchSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.utils.time.DateFormatter;
import org.slf4j.Logger;
import reactor.util.annotation.NonNull;

import java.util.Arrays;
import java.util.Date;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class SqlUtils {

    // ? 缓存,避免重复创建
    // -Deasyorm.question-marks.cache-size=200
    static final SqlFragments[] Q_M_CACHE;

    static {
        int defaultSize = Runtime.getRuntime().maxMemory() > 4 * 1024 * 1024 * 1024L ? 500 : 200;
        Q_M_CACHE = new SqlFragments[Integer.getInteger("easyorm.question-marks.cache-size", defaultSize) + 1];
        for (int i = 0; i < Q_M_CACHE.length; i++) {
            String[] arr = new String[i];
            Arrays.fill(arr, "?");
            Q_M_CACHE[i] = SqlFragments.single(String.join(",", arr));
        }
    }

    /**
     * 创建连续的预编译参数占位符的SQL片段,通常用于 in(?,?)等操作.
     *
     * @param len 长度
     * @return SqlFragments
     */
    public static SqlFragments createQuestionMarks(int len) {
        if (len == 0) {
            return EmptySqlFragments.INSTANCE;
        }
        int cacheSize = Q_M_CACHE.length;
        if (len < cacheSize) {
            return Q_M_CACHE[len];
        }
        // 从1开始
        cacheSize -= 1;
        int size = len / cacheSize;
        int remainder = len % cacheSize;

        BatchSqlFragments batch = new BatchSqlFragments(
            size * 2 + (remainder > 0 ? 1 : 0), 0);

        for (int i = 0; i < size; i++) {
            if (i > 0) {
                batch.add(SqlFragments.COMMA);
            }
            batch.add(Q_M_CACHE[cacheSize]);
        }
        if (remainder > 0) {
            if (size > 0) {
                batch.add(SqlFragments.COMMA);
            }
            batch.add(Q_M_CACHE[remainder]);
        }
        return batch;
    }

    /**
     * 将SQL参数转为字符串,通常用于打印sql参数等操作.
     *
     * @param parameters 参数
     * @return 字符串
     */
    public static String sqlParameterToString(Object[] parameters) {
        if (parameters == null) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        int i = 0;
        for (Object param : parameters) {
            if (i++ != 0) {
                builder.append(",");
            }
            builder.append(param);
            if (!(param instanceof NullValue)) {
                builder.append("(");
                builder.append(param == null ? "null" : param.getClass().getSimpleName());
                builder.append(")");
            }
        }
        return builder.toString();
    }

    /**
     * 打印sql
     *
     * @param log        日志
     * @param sqlRequest sql
     */
    public static void printSql(Logger log, SqlRequest sqlRequest) {
        if (log.isDebugEnabled()) {
            if (sqlRequest.isNotEmpty()) {
                boolean hasParameter = sqlRequest.getParameters() != null && sqlRequest.getParameters().length > 0;

                log.debug("==>  {}: {}", hasParameter ? "Preparing" : "  Execute", sqlRequest.getSql());
                if (hasParameter) {
                    log.debug("==> Parameters: {}", sqlParameterToString(sqlRequest.getParameters()));
                    if (sqlRequest instanceof PrepareSqlRequest) {
                        log.debug("==>     Native: {}", sqlRequest.toNativeSql());
                    }
                }
            }
        }
    }

    /**
     * 将SQL和参数转换为原生sql语句,通常用于日志打印.
     *
     * @param sql        SQL
     * @param parameters 预编译参数
     * @return SQL语句
     */
    public static String toNativeSql(String sql, Object... parameters) {
        if (parameters == null) {
            return sql;
        }

        String[] stringParameter = new String[parameters.length];
        int len = 0;
        for (int i = 0; i < parameters.length; i++) {
            Object parameter = parameters[i];
            if (parameter instanceof Number
                || parameter instanceof Boolean) {
                stringParameter[i] = parameter.toString();
            } else if (parameter instanceof Date) {
                stringParameter[i] = "'" + DateFormatter.toString(((Date) parameter), "yyyy-MM-dd HH:mm:ss") + "'";
            } else if (parameter instanceof NullValue) {
                stringParameter[i] = "null";
            } else if (parameter == null) {
                stringParameter[i] = "null";
            } else {
                stringParameter[i] = "'" + parameter + "'";
            }
            len += stringParameter[i].length();
        }
        return sqlParameterToString(sql, len, stringParameter);
    }

    private static @NonNull String sqlParameterToString(String sql, int len, String[] stringParameter) {
        return replaceSqlParameter(sql, len, (parameterIndex) -> {
            if (stringParameter.length > parameterIndex) {
                return stringParameter[parameterIndex];
            } else {
                return "unbound";
            }
        });
    }

    public static String replaceSqlParameter(String sql, int estimatedExtraLen, Function<Integer, String> replacer) {
        return replaceSqlParameter(
            sql,
            new StringBuilder(sql.length() + estimatedExtraLen),
            (integer, builder) -> {
                builder.append(replacer.apply(integer));
            })
            .toString();
    }

    public static StringBuilder replaceSqlParameter(String sql,
                                                    StringBuilder builder,
                                                    BiConsumer<Integer, StringBuilder> replacer) {
        int index = 0;
        boolean inSingleQuote = false;
        boolean inDoubleQuote = false;
        boolean inLineComment = false;
        boolean inBlockComment = false;

        for (int i = 0; i < sql.length(); i++) {
            char c = sql.charAt(i);

            // --- 处理注释 -----------------------------------------------------
            if (inLineComment) {
                builder.append(c);
                if (c == '\n') {
                    inLineComment = false;
                }
                continue;
            }

            if (inBlockComment) {
                builder.append(c);
                if (c == '*' && i + 1 < sql.length() && sql.charAt(i + 1) == '/') {
                    builder.append('/');
                    i++;
                    inBlockComment = false;
                }
                continue;
            }

            // 进入注释（仅当不在字符串中）
            if (!inSingleQuote && !inDoubleQuote) {
                if (c == '-' && i + 1 < sql.length() && sql.charAt(i + 1) == '-') {
                    builder.append("--");
                    i++;
                    inLineComment = true;
                    continue;
                }
                if (c == '/' && i + 1 < sql.length() && sql.charAt(i + 1) == '*') {
                    builder.append("/*");
                    i++;
                    inBlockComment = true;
                    continue;
                }
            }

            // --- 处理字符串和双引号 -------------------------------------------
            // 处理单引号字符串（支持转义：'' 表示一个单引号字符）
            if (!inDoubleQuote && c == '\'') {
                if (inSingleQuote && i + 1 < sql.length() && sql.charAt(i + 1) == '\'') {
                    // 在单引号字符串内遇到 ''，这是转义的单引号，不是字符串结束
                    builder.append("''");
                    i++; // 跳过下一个单引号
                    continue;
                }
                inSingleQuote = !inSingleQuote;
                builder.append(c);
                continue;
            }

            // 处理双引号字符串（支持转义："" 表示一个双引号字符）
            if (!inSingleQuote && c == '\"') {
                if (inDoubleQuote && i + 1 < sql.length() && sql.charAt(i + 1) == '\"') {
                    // 在双引号字符串内遇到 ""，这是转义的双引号，不是字符串结束
                    builder.append("\"\"");
                    i++; // 跳过下一个双引号
                    continue;
                }
                inDoubleQuote = !inDoubleQuote;
                builder.append(c);
                continue;
            }

            // 在字符串/标识符内不替换 '?'
            if (inSingleQuote || inDoubleQuote) {
                builder.append(c);
                continue;
            }

            // --- 跳过 PostgreSQL 操作符 ------------------------------------
            if (c == '?') {
                // 检查多字符操作符：?| ?& ?!
                if (i + 1 < sql.length()) {
                    char next = sql.charAt(i + 1);
                    if (next == '|' || next == '&' || next == '!' || next == '?') {
                        builder.append('?').append(next);
                        i++;
                        continue;
                    }
                }

                // 检查单独的 ? 操作符（PostgreSQL JSONB/数组操作符）
                // 格式：column ? 'key' 或 column ? array[...]
                // 判断条件：前面是标识符字符，后面是空格+单引号或 array
                if (isPostgresOperator(sql, i)) {
                    builder.append('?');
                    continue;
                }
            }

            // --- 在这里替换 '?' 参数 -------------------------------------------
            if (c == '?') {
                replacer.accept(index++, builder);
                continue;
            }

            // 默认追加
            builder.append(c);
        }

        return builder;
    }

    /**
     * 判断当前位置的 '?' 是否是 PostgreSQL 操作符（如 JSONB 的 ? 操作符）
     * <p>
     * PostgreSQL 操作符格式：
     * - jsonb_column ? 'key'
     * - jsonb_column ? array['key1', 'key2']
     * <p>
     * 判断逻辑：
     * 1. 前面（跳过空格）必须是标识符字符（字母、数字、下划线、右括号、右方括号或字段名引号）
     * 2. 后面（跳过空格）必须是单引号字符串、参数占位符?或 array[
     *
     * @param sql   SQL 语句
     * @param index '?' 的位置
     * @return 如果是操作符返回 true，否则返回 false
     */
    private static boolean isPostgresOperator(String sql, int index) {
        // 检查前面是否有标识符字符（跳过空格）
        int prevIndex = index - 1;
        while (prevIndex >= 0 && Character.isWhitespace(sql.charAt(prevIndex))) {
            prevIndex--;
        }

        if (prevIndex < 0) {
            // 如果 ? 在开头或前面只有空格，不可能是操作符
            return false;
        }

        char prev = sql.charAt(prevIndex);
        // 标识符字符：字母、数字、下划线、右括号、右方括号、引号
        // 如果不是这些字符，则不是操作符（可能是 =, >, < 等操作符后的参数占位符）
        if (!(Character.isLetterOrDigit(prev)
            || prev == '"'
            || prev == '_'
            || prev == ')'
            || prev == ']')) {
            return false;
        }

        // 检查后面是否是操作符格式
        if (index + 1 >= sql.length()) {
            return false;
        }

        // 跳过空格
        int nextIndex = index + 1;
        while (nextIndex < sql.length() && Character.isWhitespace(sql.charAt(nextIndex))) {
            nextIndex++;
        }

        if (nextIndex >= sql.length()) {
            return false;
        }

        char next = sql.charAt(nextIndex);

        // 检查是否是单引号字符串（'key'）或 参数占位符 （?）
        if (next == '\'' || next == '?') {
            return true;
        }

        // 检查是否是 array[...]
        if (nextIndex + 5 <= sql.length()) {
            String nextStr = sql.substring(nextIndex, Math.min(nextIndex + 5, sql.length()));
            return nextStr.toLowerCase().startsWith("array");
        }

        return false;
    }
}
