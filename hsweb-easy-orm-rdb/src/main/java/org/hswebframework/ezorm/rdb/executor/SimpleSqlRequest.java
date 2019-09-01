package org.hswebframework.ezorm.rdb.executor;

import lombok.*;
import org.hswebframework.utils.time.DateFormatter;

import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor(staticName = "of")
public class SimpleSqlRequest implements SqlRequest {

    private String sql;

    private Object[] parameters;

    public String toNativeSql() {

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

    @Override
    public String toString() {
        return toNativeSql();
    }
}
