package org.hswebframework.ezorm.core.utils;

import io.netty.util.concurrent.FastThreadLocal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class StringUtils {

    private static final FastThreadLocal<StringBuilder> SHARE = new FastThreadLocal<StringBuilder>() {
        @Override
        protected StringBuilder initialValue() throws Exception {
            return new StringBuilder();
        }
    };

    private static final FastThreadLocal<List<String>> SHARE_SPLIT = new FastThreadLocal<List<String>>() {
        @Override
        protected List<String> initialValue() {
            return new ArrayList<>(8);
        }
    };

    public static boolean isNullOrEmpty(Object arg) {
        if (arg == null) {
            return true;
        }
        if (arg instanceof String) {
            return (String.valueOf(arg)).isEmpty();
        }
        if (arg instanceof Collection) {
            return ((Collection<?>) arg).isEmpty();
        }
        if (arg instanceof Map) {
            return ((Map<?, ?>) arg).isEmpty();
        }
        return false;
    }

    public static String join(CharSequence delimiter, Iterable<?> args) {
        StringBuilder builder = SHARE.get();
        try {
            int idx = 0;
            for (Object arg : args) {
                if (idx++ > 0) {
                    builder.append(delimiter);
                }
                builder.append(arg);
            }
            return builder.toString();
        } finally {
            builder.setLength(0);
        }
    }

    public static String concat(Object... args) {
        StringBuilder builder = SHARE.get();
        try {
            for (Object arg : args) {
                builder.append(arg);
            }
            return builder.toString();
        } finally {
            builder.setLength(0);
        }
    }

    public static String toLowerCaseFirstOne(String str) {
        if (Character.isLowerCase(str.charAt(0)))
            return str;
        else {
            char[] chars = str.toCharArray();
            chars[0] = Character.toLowerCase(chars[0]);
            return new String(chars);
        }
    }

    public static String[] split(String str, char c) {
        List<String> list = SHARE_SPLIT.get();
        StringBuilder builder = SHARE.get();
        try {
            int len = str.length();
            int total = 0;

            for (int i = 0; i < len; i++) {
                char ch = str.charAt(i);
                if (ch == c) {
                    list.add(builder.toString());
                    builder.setLength(0);
                    total++;
                } else {
                    builder.append(ch);
                }
            }

            if (builder.length() > 0) {
                list.add(builder.toString());
                total++;
            }

            return list.toArray(new String[total]);
        } finally {
            builder.setLength(0);
            list.clear();
        }
    }

    public static String[] getPlainName(String[] name) {
        for (int i = 0; i < name.length; i++) {
            name[i] = getPlainName(name[i]);
        }
        return name;
    }

    public static String getPlainName(String name) {
        char str = name.charAt(0);
        if (str == '`' || str == '\"' || str == '[') {
            return name.substring(1, name.length() - 1);
        }
        return name;
    }
}
