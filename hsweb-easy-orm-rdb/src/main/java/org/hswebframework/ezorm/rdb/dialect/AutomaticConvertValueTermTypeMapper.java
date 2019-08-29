package org.hswebframework.ezorm.rdb.dialect;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
//import org.hswebframework.ezorm.rdb.render.SqlAppender;

import java.util.*;
import java.util.stream.Collectors;


public interface AutomaticConvertValueTermTypeMapper extends TermTypeMapper {

//    static AutomaticConvertValueTermTypeMapper supportArray(TermTypeMapper mapper) {
//        return new AutomaticConvertValueTermTypeMapper() {
//            @Override
//            public boolean supportArray() {
//                return true;
//            }
//
//            @Override
//            public SqlAppender accept(String wherePrefix, Term term, RDBColumnMetadata column, String tableAlias) {
//                convertValue(column, term);
//                return mapper.accept(wherePrefix, term, column, tableAlias);
//            }
//        };
//    }
//
//    static AutomaticConvertValueTermTypeMapper notSupportArray(TermTypeMapper mapper) {
//        return new AutomaticConvertValueTermTypeMapper() {
//            @Override
//            public boolean supportArray() {
//                return false;
//            }
//
//            @Override
//            public SqlAppender accept(String wherePrefix, Term term, RDBColumnMetadata column, String tableAlias) {
//                convertValue(column, term);
//                return mapper.accept(wherePrefix, term, column, tableAlias);
//            }
//        };
//    }

    boolean supportArray();

    default void convertValue(RDBColumnMetadata column, Term term) {
        if (supportArray()) {
            term.setValue(convertList(column, term.getValue()));
        } else {
            term.setValue(convertValue(column, term.getValue()));
        }
    }

    @SuppressWarnings("unchecked")
    static Object convertValue(RDBColumnMetadata column, Object value) {
        if (null == value) return null;
        Object newValue = null;
        if (column.getDictionaryCodec() != null) {
            newValue = column.getDictionaryCodec().encode(value);
        }
        if (column.getValueCodec() != null) {
            if (newValue instanceof Collection) {
                newValue = ((Collection) newValue).stream()
                        .map(column.getValueCodec()::encode)
                        .collect(Collectors.toList());
            } else {
                newValue = column.getValueCodec().encode(value);
            }
        }
        if (newValue != null) return newValue;
        return value;
    }

    static List<Object> convertList(Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List) value;
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));
        if (value instanceof String) {
            String[] arr = ((String) value).split("[,]");
            Object[] objArr = new Object[arr.length];
            for (int i = 0; i < arr.length; i++) {
                String str = arr[i];
                Object val = str;
                objArr[i] = val;
            }
            return new ArrayList<>(Arrays.asList(objArr));
        } else if (value.getClass().isArray()) {
            return new ArrayList<>(Arrays.asList(((Object[]) value)));
        } else {
            return new ArrayList<>(Collections.singletonList(value));
        }
    }

    @SuppressWarnings("unchecked")
    static List<Object> convertList(RDBColumnMetadata column, Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List) value;
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));
        if (value instanceof String) {
            String[] arr = ((String) value).split("[,]");
            Object[] objArr = new Object[arr.length];
            for (int i = 0; i < arr.length; i++) {
                String str = arr[i];
                Object val = str;
                objArr[i] = convertValue(column, val);
            }
            return new ArrayList<>(Arrays.asList(objArr));
        } else if (value.getClass().isArray()) {
            return new ArrayList<>(Arrays.asList(((Object[]) value)));
        } else {
            return new ArrayList<>(Collections.singletonList(value));
        }
    }
}