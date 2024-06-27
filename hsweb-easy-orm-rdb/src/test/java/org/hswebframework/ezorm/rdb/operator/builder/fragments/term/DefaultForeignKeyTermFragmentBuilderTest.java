package org.hswebframework.ezorm.rdb.operator.builder.fragments.term;

import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;

public class DefaultForeignKeyTermFragmentBuilderTest {


    public DefaultForeignKeyTermFragmentBuilder builder;

    public ForeignKeyMetadata foreignKeyMetadata;

    @Before
    public void init() {


        RDBSchemaMetadata metadata = MetadataHelper.createMockSchema();


        builder = new DefaultForeignKeyTermFragmentBuilder();

        metadata.getTableOrView("test")
                .ifPresent(test -> foreignKeyMetadata =
                        test.addForeignKey(ForeignKeyBuilder.builder()
                                .name("test")
                                .target("detail")
                                .terms(Collections.singletonList(new Term().and("id", "1234").or("id", "12345")))
                                .build()
//                                .addMiddle(ForeignKeyBuilder.builder()
//                                        .name("test2")
//                                        .target("test")
//                                        .source("detail")
//                                        .build().addColumn("comment","id"))
                                .addColumn("id","id")));
    }

    @Test
    public void test() {
        SqlFragments fragments = builder
                .createFragments("test", foreignKeyMetadata,
                        Query.of().where("detail.comment", 1)
                                .getParam().getTerms());

        SqlRequest request = fragments.toRequest();
        System.out.println(fragments.toRequest());
        Assert.assertEquals(request.getSql(), "exists( select 1 from \"PUBLIC\".detail detail where test.\"ID\" = detail.\"ID\" and ( ( detail.\"ID\" = ? or detail.\"ID\" = ? ) and ( detail.\"COMMENT\" = ? ) ) )");

    }

}