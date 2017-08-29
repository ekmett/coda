"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const assert = require("assert");
suite("extension tests", () => {
    test("missing", () => {
        assert.equal(-1, [1, 2, 3].indexOf(5));
        assert.equal(-1, [1, 2, 3].indexOf(0));
    });
});
//# sourceMappingURL=extension.test.js.map