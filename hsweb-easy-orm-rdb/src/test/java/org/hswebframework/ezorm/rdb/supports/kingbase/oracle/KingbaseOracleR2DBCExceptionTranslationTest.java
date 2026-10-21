package org.hswebframework.ezorm.rdb.supports.kingbase.oracle;

import io.r2dbc.spi.R2dbcDataIntegrityViolationException;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.junit.Assert;
import org.junit.Test;

/**
 * Verifies duplicate-key conversion for the Kingbase Oracle R2DBC contract.
 */
public class KingbaseOracleR2DBCExceptionTranslationTest {

    @Test
    public void shouldTranslateKingbaseDuplicateKeySqlState() {
        R2dbcDataIntegrityViolationException source =
            new R2dbcDataIntegrityViolationException("duplicate key", "23505", 0);
        System.out.println("[异常转换] 模式=Oracle，SQLState=" + source.getSqlState()
            + "，输入=duplicate key");

        Throwable translated = KingbaseOracleR2DBCExceptionTranslation
            .of(new KingbaseOracleSchemaMetadata("public"))
            .translate(source);

        Assert.assertTrue(translated instanceof DuplicateKeyException);
        Assert.assertSame(source, translated.getCause());
        System.out.println("[异常转换] 结果=" + translated.getClass().getSimpleName()
            + "，cause保留=" + (translated.getCause() == source));
    }
}
