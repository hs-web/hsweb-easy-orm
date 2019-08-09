package org.hswebframework.easyorm.elasticsearch.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.hswebframework.ezorm.core.param.Term;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * @author Jia_RG
 */
@Getter
@AllArgsConstructor
enum TermTypeEnum {
    eq("eq") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.termQuery(term.getColumn().trim(), term.getValue());
        }
    },
    not("not") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.boolQuery().mustNot(QueryBuilders.termQuery(term.getColumn().trim(), term.getValue()));
        }
    },
    btw("btw") {
        @Override
        public QueryBuilder process(Term term) {
            String[] btw = String.valueOf(term.getValue()).split("[,]");
            return QueryBuilders.rangeQuery(term.getColumn().trim()).gte(btw[0].trim()).lte(btw[1].trim());
        }
    },
    gt("gt") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.rangeQuery(term.getColumn().trim()).gt(term.getValue());
        }
    },
    gte("gte") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.rangeQuery(term.getColumn().trim()).gte(term.getValue());
        }
    },
    lt("lt") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.rangeQuery(term.getColumn().trim()).lt(term.getValue());
        }
    },
    lte("lte") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.rangeQuery(term.getColumn().trim()).lte(term.getValue());
        }
    },
    in("in") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.termsQuery(term.getColumn().trim(), convertToList(term.getValue()));
        }
    },
    like("like") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.matchPhraseQuery(term.getColumn().trim(), term.getValue());
        }
    },
    nlike("nlike") {
        @Override
        public QueryBuilder process(Term term) {
            return QueryBuilders.boolQuery().mustNot(QueryBuilders.matchPhraseQuery(term.getColumn().trim(), term.getValue()));
        }
    };

    private final String type;

    public abstract QueryBuilder process(Term term);

    public static Optional<TermTypeEnum> of(String type) {
        return Arrays.stream(values())
                .filter(e -> e.getType().equalsIgnoreCase(type))
                .findAny();
    }

    private static List<Object> convertToList(Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List<Object>) value;
        if (value instanceof Collection) return new ArrayList<Object>(((Collection) value));
        if (value instanceof String) {
            String[] arr = ((String) value).split("[,]");
            Object[] objArr = new Object[arr.length];
            System.arraycopy(arr, 0, objArr, 0, arr.length);
            return new ArrayList<>(Arrays.asList(objArr));
        } else if (value.getClass().isArray()) {
            return new ArrayList<>(Arrays.asList(((Object[]) value)));
        } else {
            return new ArrayList<>(Collections.singletonList(value));
        }
    }
}
