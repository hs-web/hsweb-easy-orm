package org.hswebframework.ezorm.rdb.codec;

import org.hswebframework.utils.DateTimeUtils;
import org.hswebframework.utils.time.DateFormatter;
import org.hswebframework.ezorm.core.ValueCodec;

import java.util.Arrays;
import java.util.Date;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 日期转换器
 */
public class DateTimeCodec implements ValueCodec {

    private String format;

    private Class toType;

    public DateTimeCodec(String format, Class toType) {
        this.format = format;
        this.toType = toType;
    }

    @Override
    public Object encode(Object value) {
        if (value instanceof Date) return value;
        if (value instanceof String) {
            if (((String) value).contains(",")) {
                return Arrays.stream(((String) value).split(","))
                        .map(DateFormatter::fromString)
                        .collect(Collectors.toList());
            }
            return DateFormatter.fromString(((String) value));
        }
        return value;
    }

    @Override
    public Object decode(Object data) {
        if (data instanceof Number) {
            data = new Date(((Number) data).longValue());
        }
        if (data instanceof Date) {
            if (toType == Date.class) {
                return data;
            }
            if (toType == String.class) {
                return DateTimeUtils.format(((Date) data), format);
            }
        }
        if (data instanceof String) {
            String stringData = ((String) data);
            if (toType == Date.class) {
                if ((stringData).contains(",")) {
                    return Arrays.stream(stringData.split(","))
                            .map(DateFormatter::fromString)
                            .filter(Objects::nonNull)
                            .collect(Collectors.toList());
                }
                data = DateFormatter.fromString(stringData);
                if (data == null) {
                    data = DateTimeUtils.formatDateString(stringData, format);
                }
            }
        }
        return data;
    }
}
