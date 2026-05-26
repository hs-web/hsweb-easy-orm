package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.junit.Assert;
import org.junit.Test;

public class QueryTermsFragmentBuilderStrictModeTest {

    @Test(expected = UnsupportedOperationException.class)
    public void testStrictModeFailsOnUnsupportedJsonTerm() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setFrom("test");
        parameter.setContext(java.util.Collections.singletonMap("easyorm.strict.term", true));
        parameter.getWhere().add(Term.of("name", "json_value", java.util.Collections.singletonMap("path", "age")));

        TableOrViewMetadata table = MetadataHelper.createMockSchema().getTable("test").orElseThrow(IllegalStateException::new);
        QueryTermsFragmentBuilder.of(table)
                                 .createFragments(parameter);
    }

    @Test
    public void testNonStrictModeKeepsBackwardCompatibility() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setFrom("test");
        parameter.getWhere().add(Term.of("name", "json_value", java.util.Collections.singletonMap("path", "age")));

        TableOrViewMetadata table = MetadataHelper.createMockSchema().getTable("test").orElseThrow(IllegalStateException::new);
        Assert.assertTrue(QueryTermsFragmentBuilder.of(table)
                                                  .createFragments(parameter)
                                                  .isEmpty());
    }
}
