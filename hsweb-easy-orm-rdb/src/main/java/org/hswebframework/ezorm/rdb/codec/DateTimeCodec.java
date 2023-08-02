package org.hswebframework.ezorm.rdb.codec;

import lombok.NoArgsConstructor;
import org.hswebframework.utils.DateTimeUtils;
import org.hswebframework.utils.time.DateFormatter;
import org.hswebframework.ezorm.core.ValueCodec;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;

import java.sql.Timestamp;
import java.time.*;
import java.util.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * 日期转换器
 */
@NoArgsConstructor
public class DateTimeCodec implements ValueCodec {

    private String format;

    private Class toType;

    public DateTimeCodec(String format, Class toType) {
        this.format = format;
        this.toType = toType;
    }

    protected Date doParse(String dateString) {
        Date smartFormat = DateFormatter.fromString(dateString);
        if (smartFormat != null) {
            return smartFormat;
        }
        return DateTimeFormat.forPattern(format).parseDateTime(dateString).toDate();
    }

    @Override
    public Object encode(Object value) {
        if (value instanceof Date) {
            return value;
        }
        if (value instanceof Number) {
            return new Date(((Number) value).longValue());
        }
        if (value instanceof String) {
            if (((String) value).contains(",")) {
                return Arrays.stream(((String) value).split(","))
                             .map(this::doParse)
                             .filter(Objects::nonNull)
                             .collect(Collectors.toList());
            }

            return doParse(((String) value));
        }
        return value;
    }

    @Override
    @SuppressWarnings("all")
    public Object decode(Object data) {
        if (null == data) {
            return null;
        }
        if (toType.isAssignableFrom(data.getClass())) {
            return data;
        }
        if (data instanceof Number) {
            data = new Date(((Number) data).longValue());
        } else if (data instanceof Timestamp) {
            data = Date.from(((Timestamp) data).toInstant());
        } else if (data instanceof Instant) {
            data = Date.from(((Instant) data));
        } else if (data instanceof LocalDateTime) {
            LocalDateTime dateTime = ((LocalDateTime) data);
            data = Date.from(dateTime.atZone(ZoneOffset.systemDefault()).toInstant());
        } else if (data instanceof ZonedDateTime) {
            ZonedDateTime dateTime = ((ZonedDateTime) data);
            data = Date.from(dateTime.toInstant());
        }
        if (data instanceof Date) {
            if (toType == Date.class) {
                return data;
            }
            if (toType == Instant.class) {
                return ((Date) data).toInstant();
            }
            if (toType == LocalDateTime.class) {
                return LocalDateTime.ofInstant(((Date) data).toInstant(), ZoneId.systemDefault());
            }
            if (toType == LocalDate.class) {
                return LocalDateTime
                        .ofInstant(((Date) data).toInstant(), ZoneId.systemDefault())
                        .toLocalDate();
            }
            if (toType == LocalTime.class) {
                return LocalDateTime
                        .ofInstant(((Date) data).toInstant(), ZoneId.systemDefault())
                        .toLocalTime();
            }
            if (toType == String.class) {
                return DateTimeUtils.format(((Date) data), format);
            }
        }
        if (data instanceof Collection) {
            return ((Collection<?>) data)
                    .stream()
                    .map(this::decode)
                    .collect((Collector) (toType == String.class ? Collectors.joining(",") : Collectors.toList()));

        }
        if (data instanceof String) {
            String stringData = ((String) data);
            if (toType == Date.class) {
                if ((stringData).contains(",")) {
                    return Arrays
                            .stream(stringData.split(","))
                            .map(this::doParse)
                            .filter(Objects::nonNull)
                            .collect(Collectors.toList());
                }
                data = this.doParse(stringData);
            }
        }
        return data;
    }
}
