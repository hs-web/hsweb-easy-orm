package org.hswebframework.easyorm.elasticsearch;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.hswebframework.easyorm.elasticsearch.enums.LinkTypeEnum;
import org.hswebframework.ezorm.core.param.QueryParam;

import java.util.Objects;

/**
 * @author zhou-hao
 * @author Jia_RG
 */
@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ElasticSearchQueryParamTranslator {

    public static QueryBuilder translate(QueryParam queryParam) {
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        Objects.requireNonNull(queryParam, "QueryParam must not null.")
                .getTerms()
                .forEach(term -> LinkTypeEnum.of(term.getType().name())
                        .ifPresent(e -> e.process(query, term)));
        return query;
    }
}
