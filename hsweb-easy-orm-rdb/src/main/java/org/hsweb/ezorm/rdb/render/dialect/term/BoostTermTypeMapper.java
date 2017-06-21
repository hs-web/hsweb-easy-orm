package org.hsweb.ezorm.rdb.render.dialect.term;

import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;

import java.util.*;
import java.util.stream.Collectors;


public interface BoostTermTypeMapper extends Dialect.TermTypeMapper {

    static BoostTermTypeMapper supportArray(Dialect.TermTypeMapper mapper) {
        return new BoostTermTypeMapper() {
            @Override
            public boolean supportArray() {
                return true;
            }

            @Override
            public SqlAppender accept(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias) {
                transformationValue(column, term);
                return mapper.accept(wherePrefix, term, column, tableAlias);
            }
        };
    }

    static BoostTermTypeMapper notSupportArray(Dialect.TermTypeMapper mapper) {
        return new BoostTermTypeMapper() {
            @Override
            public boolean supportArray() {
                return false;
            }

            @Override
            public SqlAppender accept(String wherePrefix, Term term, RDBColumnMetaData column, String tableAlias) {
                transformationValue(column, term);
                return mapper.accept(wherePrefix, term, column, tableAlias);
            }
        };
    }

    boolean supportArray();

    default void transformationValue(RDBColumnMetaData column, Term term) {
        if (supportArray()) {
            term.setValue(convertList(column, term.getValue()));
        } else {
            term.setValue(convertValue(column, term.getValue()));
        }
    }

    @SuppressWarnings("unchecked")
    static Object convertValue(RDBColumnMetaData column, Object value) {
        if (null == value) return null;
        Object newValue = null;
        if (column.getOptionConverter() != null) {
            newValue = column.getOptionConverter().converterData(value);
        }
        if (column.getValueConverter() != null) {
            if (newValue instanceof Collection) {
                newValue = ((Collection) newValue).stream()
                        .map(column.getValueConverter()::getData)
                        .collect(Collectors.toList());
            } else {
                newValue = column.getValueConverter().getData(value);
            }
        }
        if (newValue != null) return newValue;
        return value;
    }

    @SuppressWarnings("unchecked")
    static List<Object> convertList(RDBColumnMetaData column, Object value) {
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