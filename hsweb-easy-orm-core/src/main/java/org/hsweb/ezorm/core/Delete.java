package org.hsweb.ezorm.core;

import org.hsweb.ezorm.core.param.Param;

import java.sql.SQLException;

public interface Delete extends Conditional<Delete>, TriggerSkipSupport<Delete> {
    Delete setParam(Param param);

    int exec() throws SQLException;
}
