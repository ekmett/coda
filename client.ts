'use strict';
import * as path from 'path';
import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind } from 'vscode-languageclient';
export function activate(context: ExtensionContext) {
  let serverModule = context.asAbsolutePath(path.join('.cabal-sandbox','bin','codad'));
  let debugOptions = { execArgv: ["--debug"] };
  let serverOptions: ServerOptions = {
    run : { module: serverModule, transport: TransportKind.ipc },
    debug: { module: serverModule, transport: TransportKind.ipc, options: debugOptions }
  }
  let clientOptions: LanguageClientOptions = {
    documentSelector: ['coda'],
    synchronize: {
      configurationSection: 'codad',
      fileEvents: workspace.createFileSystemWatcher('**/.codarc')
    }
  }
  let disposable = new LanguageClient('codad', 'coda language server', serverOptions, clientOptions).start();
  context.subscriptions.push(disposable);
}
