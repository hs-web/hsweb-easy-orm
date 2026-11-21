package org.hswebframework.ezorm.rdb.xa;

import com.kingbase8.xa.KBXADataSource;
import org.junit.Test;

import static org.junit.Assert.*;

public class XADataSourceUtilTest {

    @Test
    public void createsOfficialKingbaseXaDataSourceFromConnectionSettings() {
        KBXADataSource source = XADataSourceUtil.createKingbase(
            "jdbc:kingbase8://127.0.0.1:54321/test", "system", "test-password");

        assertEquals("127.0.0.1", source.getServerNames()[0]);
        assertEquals(54321, source.getPortNumbers()[0]);
        assertEquals("test", source.getDatabaseName());
        assertEquals("system", source.getUser());
        assertEquals("test-password", source.getPassword());
    }
}
