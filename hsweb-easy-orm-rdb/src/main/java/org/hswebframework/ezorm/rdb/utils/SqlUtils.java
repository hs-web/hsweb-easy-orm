package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.EmptySqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.utils.time.DateFormatter;
import org.slf4j.Logger;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.Date;

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
        if (len < Q_M_CACHE.length) {
            return Q_M_CACHE[len];
        }
        String[] arr = new String[len];
        Arrays.fill(arr, "?");
        return SqlFragments.single(String.join(",", arr));
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
            len += stringParameter.length;
        }
        return sqlParameterToString(sql, len, stringParameter);
    }

    private static @Nonnull String sqlParameterToString(String sql, int len, String[] stringParameter) {
        StringBuilder builder = new StringBuilder(sql.length() + len + 16);

        int parameterIndex = 0;
        for (int i = 0, sqlLen = sql.length(); i < sqlLen; i++) {
            char c = sql.charAt(i);
            if (c == '?') {
                if (stringParameter.length > parameterIndex) {
                    builder.append(stringParameter[parameterIndex++]);
                } else {
                    builder.append("unbound");
                }
            } else {
                builder.append(c);
            }
        }
        return builder.toString();
    }
}
