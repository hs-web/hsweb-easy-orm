package org.hswebframework.ezorm.core;

import org.hswebframework.ezorm.core.param.Param;

import java.sql.SQLException;

public interface Delete extends Conditional<Delete>, TriggerSkipSupport<Delete> {
    Delete setParam(Param param);

    int exec() throws SQLException;
}
