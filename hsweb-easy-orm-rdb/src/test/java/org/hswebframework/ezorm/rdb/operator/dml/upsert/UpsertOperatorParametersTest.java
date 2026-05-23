package org.hswebframework.ezorm.rdb.operator.dml.upsert;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

public class UpsertOperatorParametersTest {

    @Test
    public void testAppendRuntimeDefaultPrimaryKeyWhenMissing() {
        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("test"));

        UpsertOperatorParameter result = UpsertOperatorParameters
            .ensureRuntimeDefaultPrimaryKey(parameter, primaryKey("id", "id"));

        Assert.assertNotSame(parameter, result);
        Assert.assertEquals(2, result.getColumns().size());
        Assert.assertEquals(2, result.getValues().get(0).size());
    }

    @Test
    public void testDoNotAppendWhenPrimaryKeyNameMatchesIgnoreCase() {
        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("ID", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("fixed-id", "test"));

        UpsertOperatorParameter result = UpsertOperatorParameters
            .ensureRuntimeDefaultPrimaryKey(parameter, primaryKey("id", "id"));

        Assert.assertSame(parameter, result);
        Assert.assertEquals(2, result.getColumns().size());
        Assert.assertEquals(2, result.getValues().get(0).size());
    }

    @Test
    public void testDoNotAppendWhenPrimaryKeyAliasMatchesIgnoreCase() {
        UpsertOperatorParameter parameter = new UpsertOperatorParameter();
        parameter.getColumns().add(UpsertColumn.of("ID_ALIAS", false));
        parameter.getColumns().add(UpsertColumn.of("name", false));
        parameter.getValues().add(Arrays.asList("fixed-id", "test"));

        UpsertOperatorParameter result = UpsertOperatorParameters
            .ensureRuntimeDefaultPrimaryKey(parameter, primaryKey("id", "id_alias"));

        Assert.assertSame(parameter, result);
        Assert.assertEquals(2, result.getColumns().size());
        Assert.assertEquals(2, result.getValues().get(0).size());
    }

    private static RDBColumnMetadata primaryKey(String name, String alias) {
        RDBColumnMetadata column = new RDBColumnMetadata();
        column.setName(name);
        column.setAlias(alias);
        column.setPrimaryKey(true);
        column.setDefaultValue(DefaultValue.runtime("generated-id"));
        return column;
    }
}
