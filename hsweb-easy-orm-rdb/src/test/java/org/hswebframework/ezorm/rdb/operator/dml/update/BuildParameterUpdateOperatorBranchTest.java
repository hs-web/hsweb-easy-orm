package org.hswebframework.ezorm.rdb.operator.dml.update;

import org.junit.Assert;
import org.junit.Test;

public class BuildParameterUpdateOperatorBranchTest {
    @Test
    public void testSetAndWhereBranches() {
        BuildParameterUpdateOperator update = new BuildParameterUpdateOperator();
        UpdateColumn direct = new UpdateColumn();
        direct.setColumn("name");
        direct.setValue("123");
        update.set(direct);
        update.set(java.util.Map.of("age", 18));
        update.where(term -> term.and("id", "1"));
        update.where(() -> org.hswebframework.ezorm.core.param.Term.of("age", "gt", 10));
        Assert.assertEquals(2, update.getParameter().getColumns().size());
        Assert.assertEquals(2, update.getParameter().getWhere().size());
        try {
            update.set((Object) new Object());
            Assert.fail();
        } catch (UnsupportedOperationException ignore) {}
    }
}
