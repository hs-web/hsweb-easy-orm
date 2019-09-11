package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.utils.time.DateFormatter;
import org.slf4j.Logger;

import java.util.Date;

public class SqlUtils {

    public static String sqlParameterToString(Object[] parameters) {
        if (parameters == null) {
            return "";
        }
        StringBuilder builder = new StringBuilder();
        int i = 0;
        for (Object param : parameters) {
            if (i++ != 0)
                builder.append(",");
            builder.append(param);
            builder.append("(");
            builder.append(param == null ? "null" : param.getClass().getSimpleName());
            builder.append(")");
        }
        return builder.toString();
    }


    public static void printSql(Logger log, SqlRequest sqlRequest) {
        if (log.isDebugEnabled()) {
            if (sqlRequest.isNotEmpty()) {
                boolean hasParameter = sqlRequest.getParameters() != null && sqlRequest.getParameters().length > 0;

                log.debug("==>  {}: {}", hasParameter ? "Preparing" : "  Execute", sqlRequest.getSql());
                if (hasParameter) {
                    log.debug("==> Parameters: {}", sqlParameterToString(sqlRequest.getParameters()));
                    if (sqlRequest instanceof PrepareSqlRequest) {
                        log.debug("==>     Native: {}", ((PrepareSqlRequest) sqlRequest).toNativeSql());
                    }
                }
            }
        }
    }


    public static String toNativeSql(String sql,Object... parameters){

        String[] stringParameter = new String[parameters.length];
        int len = 0;
        for (int i = 0; i < parameters.length; i++) {
            Object parameter = parameters[i];
            if (parameter instanceof Number) {
                stringParameter[i] = parameter.toString();
            } else if (parameter instanceof Date) {
                stringParameter[i] = "'" + DateFormatter.toString(((Date) parameter), "yyyy-MM-dd HH:mm:ss") + "'";
            } else if (parameter == null) {
                stringParameter[i] = "null";
            } else {
                stringParameter[i] = "'" + parameter + "'";
            }
            len += stringParameter.length;
        }
        StringBuilder builder = new StringBuilder(sql.length() + len + 16);

        int parameterIndex = 0;
        for (int i = 0, sqlLen = sql.length(); i < sqlLen; i++) {
            char c = sql.charAt(i);
            if (c == '?') {
                if (stringParameter.length >= parameterIndex) {
                    builder.append(stringParameter[parameterIndex++]);
                } else {
                    builder.append("null");
                }
            } else {
                builder.append(c);
            }
        }

        return builder.toString();
    }
}
